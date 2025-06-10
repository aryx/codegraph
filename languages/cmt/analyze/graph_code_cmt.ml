(* Yoann Padioleau
 *
 * Copyright (C) 2012-2014 Facebook
 * Copyright (C) 2020 Semgrep Inc.
 * Copyright (C) 2025 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Fpath_.Operators
module E = Entity_code
module G = Graph_code
open Cmt_format
open Typedtree

let debug = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for OCaml typed AST files (.cmt). See graph_code.ml
 * and src/Main.ml for more information. See also notes_cmt.txt.
 * 
 * schema:
 *  Root -> Dir -> Module -> Function
 *                        -> Type -> Constructor
 *                                -> Field
 *                        -> Exception (with .exn as prefix)
 *                        -> Constant
 *                        -> Global
 *                        -> SubModule -> ...
 * 
 * note that ocaml allows to have multiple entities with the same name
 * inside the same module, so we have to merge them; see the 'dupe_ok'
 * parameter below.
 * 
 * related:
 *  - typerex (fancy module analysis done by tiphaine turpin?)
 *  - ocamlspotter
 *  - merlin/ocamllsp
 *  - whole program analysis done by ocamlpro recently?
 *  - oug/odb http://odb-serv.forge.ocamlcore.org/
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.t;

  phase: phase;

  (* the .cmt, used mostly for error reporting, in readable path format *)
  cmt_file: Common2_.filename;
  (* the file the .cmt is supposed to come from, in readable path format *)
  ml_file: Common2_.filename;
  (* update: readable means relative to project root dir and without the
   * dune special encoding (no _build/default or .pp or .objs in it) *)

  source_finder: string (* basename *) -> Common2_.filename list;
  
  current: Graph_code.node;
  current_entity: name;

  mutable locals: string list;

  (* The cmt files do not contain the full path
   * for locally referenced functions, types, or modules, so we have to resolve
   * them. Each time you add an Ident.t, add it there, and each
   * time you use a Path.t, use path_resolve_locals().
   * We use 3 different fields because those are different namespaces; we
   * don't want a value to shadow a type.
   * See also notes_cmt.txt.
   *)
  full_path_local_type: (string * name) list ref;
  full_path_local_value: (string * name) list ref;
  (* this is less necessary because by convention modules use uppercase and
   * values/types only lowercase and so there is no shadowing risk.
   *)
  full_path_local_module: (string * name) list ref;

  (* global to the whole project, populated in Defs and used in Uses,
   * see path_resolve_aliases().
   *)
  module_aliases: (name * name) list ref;
  type_aliases: (name * name) list ref;
  
  lookup_fail: env -> Graph_code.node -> unit;
}
 (* todo: what about names which are applications of functor? 
  * See Longident.t
  *)
 and name = string list
 and phase = Defs | Uses

(* for syncweb's indexer *)
let hook_use_edge = ref (fun (_src, _dst) _g _loc -> ())
let hook_def_node = ref (fun _node _g -> ())

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* because we use a 2 passes process (should do like in PHP all in 1 pass?) *)
let _hmemo = Hashtbl.create 101
let parse (file : Common2_.filename) : Cmt_format.cmt_infos  =
  Common.memoized _hmemo file (fun () ->
    try 
      Cmt_format.read_cmt file
    with (Cmi_format.Error _) as exn ->
      failwith (spf "PB with %s, exn = %s" file (Common.exn_to_s exn))
  )

(*****************************************************************************)
(* Naming helpers *)
(*****************************************************************************)

let top_module_of_node (s, kind) =
  if s =~ "^\\([A-Z][A-Za-z0-9_]*\\)" then Common.matched1 s
  else
    failwith
      (spf "could not find top module of %s"
         (Graph_code.string_of_node (s, kind)))

let final_string_of_ident s =
  s
(* todo: does not work; get some lookup_fail
   if s =~ "^[_A-Za-z].*"
   then s
   (* operator needs extra parenthesis so we get
    * Stdlib.Pervasives.(+.) instead of Stdlib.Pervasives.+.
    *)
   else "(" ^ s ^ ")"
*)

let string_of_id id =
   let s = Ident.name id in
   final_string_of_ident s

(* TODO: starting from 4.10, the mb_id module binding can be an option *)
let string_of_id_opt idopt =
   match idopt with
   | None -> "None_NO_IDENT_TODO"
   | Some id -> string_of_id id

let s_of_n xs = 
 xs |> List.map final_string_of_ident |> String.concat "."


let (name_of_path: Path.t -> name) = fun path ->
  (* similar to Path.name *)
  let rec aux = function
  | Path.Pident id -> [string_of_id id]
  | Path.Pdot (p, s) ->
     s::aux p
  | Path.Papply(p1, _p2TODO) -> 
     aux p1
  in
  let xs = aux path |> List.rev in
  match xs with
  (* ugly: since ocaml 4.07, the compiler transforms calls to
   * List.xxx in Stdlib__list.xxx, but it does not do it for Bigarray
   * hence this hack
   *)
  | "Bigarray"::xs -> "Stdlib__bigarray"::xs
  (* ugly: moreover explicit calls to Pervasives.xxx do not get transformed
   * either in simply Stdlib.xxx, hence this second hack
   *)
  | "Stdlib"::"Pervasives"::xs -> "Stdlib"::xs
  | _ -> xs

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

let todo _env s = 
  Logs.err (fun m -> m "TODO: Graph_code_cmt %s" s)

let unwrap (x : 'a Location.loc) : Location.t = 
  x.loc

let pos_of_loc (loc : Location.t) (file : Fpath.t) : Tok.location =
  let lexing_pos = loc.Location.loc_start in
  { str = "";
    pos = {
      line = lexing_pos.Lexing.pos_lnum; 
      bytepos = lexing_pos.Lexing.pos_cnum;
      column = lexing_pos.Lexing.pos_cnum - lexing_pos.Lexing.pos_bol;
      file;
    }
  }

let (name_of_longident_loc: Longident.t Asttypes.loc -> name) = fun lidloc ->
  let lid = lidloc.Asttypes.txt in
  Longident.flatten lid

(* undo the encoding dune is doing on files to get nicer paths in codegraph
 * and codemap.
 *)
let undo_dune_mlfile file = 
  match file with
  | _ when file =~ "^\\(.*\\)/_build/default/\\(.*\\)\\.pp\\.\\(ml[i]?\\)$" ->
    let (root, dir, ext) = Common.matched3 file in
    spf "%s/%s.%s" root dir ext
  | _ when file =~ "^\\(.*\\)_build/default/\\(.*\\)\\.\\(ml[i]?\\)$" ->
    let (root, dir, ext) = Common.matched3 file in
    spf "%s%s.%s" root dir ext
  | _ -> file

let undo_dune_cmtfile file = 
  match file with
  | _ when file =~ "^\\(.*\\)_build/default/\\(.*\\)/\\.[a-zA-Z_]+\\.[e]?objs/byte/\\(.*\\)$" ->
      let (root, dir, file) = Common.matched3 file in
      spf "%s%s/%s" root dir file
  | _ -> file

let readable_path_of_ast ast root readable_cmt source_finder =
  let fullpath =
    Filename.concat ast.cmt_builddir
      (match ast.cmt_sourcefile with
      | Some file -> file
      (* ex: dynlinkaux.cmt does not have a cmt_sourcefile (weird) *)
      | None -> failwith (spf "no cmt_source_file for %s" readable_cmt)
      )
  in
  let fullpath = undo_dune_mlfile fullpath in
  (* ugly: the OCaml distribution and most of the OPAM libraries do not
   * come with .cmt files, so I have to reference them via the FOR_MERLIN
   * symlink pointing to ~/.opam/4.../.opam-switch/build/...
   * The problem is that those cmt files have realpaths in ~/.opam/
   * so Common.readable below will fail for them, hence the try
   * and the use of source_finder below.
   *)
  let readable_opt = 
    try Some (Filename_.readable ~root fullpath)
    with Failure _ -> None
  in 
  let res = 
   match readable_opt with
   (* easy case *)
   | Some readable when Sys.file_exists fullpath -> readable
   | Some _readable ->
       let candidates = ["mll"; "mly"; "dyp"] in
       let (d,b,_e) = Filename_.dbe_of_filename fullpath in
       let xs = candidates |> List.map (fun ext ->
          Filename_.filename_of_dbe (d,b,ext))
        in
       (match xs |> List.find_opt Sys.file_exists with
       | Some fullpath -> Filename_.readable ~root fullpath
       | None ->
        Logs.warn (fun m -> m "no matching source for %s, candidates = [%s]"
                  readable_cmt (xs |> String.concat ", "));
        (spf "TODO_NO_SOURCE_FOUND:%s" fullpath)
       )
       
   | None ->
    (* try our best to find the readable source *)
    (match source_finder (Filename.basename fullpath) with
    | [readable] -> readable
    | xs -> 
      (* most of the time there are multiple candidates because
       * the library or the ocaml source itself have multiple
       * versions of the same file (e.g., ocaml/otherlibs/unix/unix.ml
       * and ocaml/otherlibs/win32unix/unix.ml) in which case
       * we look wether one of the .ml is in the same dir than the .cmt
       *)
      let dir_cmt = Filename.dirname readable_cmt in
      try 
        xs |> List.find (fun file -> Filename.dirname file = dir_cmt)
      with Not_found ->
        Logs.warn (fun m -> m "no matching source for %s, candidates = [%s]"
                  readable_cmt (xs |> String.concat ", "));
        (spf "TODO_NO_SOURCE_FOUND:%s" fullpath)
    )
  in
  res

let use_of_undefined_ok name =
  name |> List.exists (function
    (* todo: need handle functor *)
    | s when s =~ ".*Set" -> true
    | s when s =~ ".*Map" -> true
    (* todo: need handle argument to functor *)
    | "MODEL" | "column_list"
    | "Taint"  | "MATCH" | "X" | "PHP_VS_PHP" | "Interp"
    (* todo: handle pack *)
    | "Digraph"
    (* todo: misc *)
    | "LIST" | "M_01_01"
      -> true
      
    | _ -> false
  )

let is_builtin_type s =
  match s with
  | "unit" | "bool" 
  | "int" | "int64" | "float" 
  | "char" | "string" | "bytes"
  | "exn" 
  | "list" | "ref" | "option" | "array" 
    -> true
  | _ -> false

(*****************************************************************************)
(* Add edges *)
(*****************************************************************************)

let add_use_edge env dst loc =
  let file = env.ml_file in
  let pos = pos_of_loc loc (Fpath.v file) in
  let src = env.current in
  if G.has_node dst env.g
  then begin 
    G.add_edge (src, dst) G.Use env.g;
    !hook_use_edge (src, dst) env.g pos;
  end
  else begin
    G.add_node dst env.g;
    !hook_def_node dst env.g;
    let parent_target = G.not_found in
    env.lookup_fail env dst;
    env.g |> G.add_edge (parent_target, dst) G.Has;
    env.g |> G.add_edge (src, dst) G.Use;
    !hook_use_edge (src, dst) env.g pos;
  end

let full_path_local_of_kind (env : env) (kind : E.kind) : (string * name) list ref =
  match kind with
  | E.Function | E.Global | E.Constant
  | E.TopStmts
    -> env.full_path_local_value
  | E.Type | E.Exception -> env.full_path_local_type
  | E.Module -> 
      (* todo: why cant put env.full_path_local_module ? *)
      env.full_path_local_type
  | E.Field | E.Constructor -> ref []
  (* todo? *)
  | E.Class -> ref []
  | E.Other _ -> ref []
  | _ -> raise Impossible

let add_full_path_local (env : env) (s, name) (kind : E.kind) : unit =
  Stack_.push (s, name) (full_path_local_of_kind env kind)

let add_node_and_edge_if_defs_mode ?(dupe_ok=false) env name_node loc =
  let (name, kind) = name_node in
  let node = (s_of_n name, kind) in
  if env.phase =*= Defs then begin
    if G.has_node node env.g && dupe_ok
    then () (* pr2 "already present entity" *)
    else begin
      env.g |> G.add_node node;
      env.g |> G.add_edge (env.current, node) G.Has;

      let file = env.ml_file in
      let pos = pos_of_loc loc (Fpath.v file) in
      let nodeinfo = { Graph_code.
         pos = pos;
         props = [];
         typ = None; (* TODO *)
         range = None;
         scip_symbol = None;
      } in
      env.g |> G.add_nodeinfo node nodeinfo;
      !hook_def_node node env.g;
    end
  end;
  add_full_path_local env (Common2.list_last name, name) kind;
  { env with 
    current = node; 
    current_entity = name; 
  }

(*****************************************************************************)
(* Path resolution, locals *)
(*****************************************************************************)

(* f --> A.f,  and Nested.f -> A.Nested.f *)
let path_resolve_locals env name kind =
(*
  let s = Path.name p in
  let xs = n_of_s s in
*)
  match name with
  | [] -> raise Impossible
  | [x] -> 
        let table = full_path_local_of_kind env kind in
        if List.mem_assoc x !table
        then List.assoc x !table
        else [x]
  | x::xs ->
        let kind = E.Module in
        let table = full_path_local_of_kind env kind in
        if List.mem_assoc x !table
        then List.assoc x !table @ xs
        else x::xs

(*****************************************************************************)
(* Path resolution, aliases *)
(*****************************************************************************)

(* algo: first resolve module aliases, then once have a full path for
 * a type, look for a type alias, and recurse.
 * opti: ?
 * todo: does it handle module shadowing? see meta_ast_cmt.ml and the issue
 * with Types module.
 *)
let rec path_type_resolve_aliases env pt =
  let rec aux module_aliases_candidates acc pt =
  match pt with
  | [] -> raise Impossible
  (* didn't found any module alias => canonical name module-wise *)
  | [t] -> List.rev (t::acc)
  | x::xs ->
      let reduced_candidates = 
        module_aliases_candidates |> List.filter_map (function
        | (y::ys, v) when x = y -> Some (ys, v)
        | _ -> None
        )
      in
      (match reduced_candidates with
      | [] -> aux [] (x::acc) xs
      (* found a unique alias *)
      | [[], v] -> 
          (* restart from the top *)
          aux !(env.module_aliases) [] (v @ xs)
      | _ ->
          aux reduced_candidates (x::acc) xs
      )
  in
  let pt = aux !(env.module_aliases) [] pt in
  if List.mem_assoc pt !(env.type_aliases)
  then path_type_resolve_aliases env (List.assoc pt !(env.type_aliases))
  else pt

let path_resolve_aliases env p =
  let rec aux module_aliases_candidates acc pt =
  match pt with
  | [] -> raise Impossible
  (* didn't found any module alias => canonical name *)
  | [x] -> List.rev (x::acc)
  | x::xs ->
      let reduced_candidates = 
        module_aliases_candidates |> List.filter_map (function
        | (y::ys, v) when x = y -> Some (ys, v)
        | _ -> None
        )
      in
      (match reduced_candidates with
      | [] -> aux [] (x::acc) xs
      (* found a unique alias *)
      | [[], v] -> 
          (* restart from the top *)
          aux !(env.module_aliases) [] (v @ xs)
      | _ ->
          aux reduced_candidates (x::acc) xs
      )
  in
  let p = aux !(env.module_aliases) [] p in
  p

(*****************************************************************************)
(* Kind of entity *)
(*****************************************************************************)
    
let rec kind_of_type_desc (x : Types.type_desc) : E.kind =
(*
  if !debug then 
  UCommon.pr2 (OCaml.string_of_v (Meta_ast_cmt.vof_type_desc x));
*)
  match x with
  | Types.Tarrow _ -> 
      E.Function
  | Types.Tconstr (path, _, _) when 
        List.mem (Path.name path) ["Pervasives.ref";"Hashtbl.t"] ->
      (* less: potentially anything with a mutable field *)
      E.Global
  (* todo: what if it is an alias to a function type? need resolve here? *)
  | Types.Tconstr (_path, _xs, _aref) -> 
      E.Constant
  | Types.Ttuple _ | Types.Tvariant _ -> 
      E.Constant
  (* ? *)
  | Types.Tvar _ -> E.Constant
  | Types.Tlink x -> kind_of_type_expr x
  | Types.Tobject _ -> E.Class
  | Types.Tpackage _ -> E.Module
  | _ -> 
      (* UCommon.pr2 (OCaml.string_of_v (Meta_ast_cmt.vof_type_desc x)); *)
      raise Todo
      
and kind_of_type_expr (x : Types.type_expr) =
  kind_of_type_desc (Types.Transient_expr.repr x).desc
    
(* used only for primitives *)
let kind_of_core_type x =
  match x.ctyp_desc with
  | Ttyp_arrow _ -> E.Function
  | Ttyp_any  | Ttyp_var _
      -> E.Other "TODO:kind_of_core_type"
  | _ -> E.Other "TODO:kind_of_core_type"

let kind_of_value_descr vd =
  kind_of_core_type vd.val_desc

(*****************************************************************************)
(* Uses with name resolution *)
(*****************************************************************************)

let typename_of_texpr ( x : Types.type_expr) =
(*
  if !debug then pr2(OCaml.string_of_v(Meta_ast_cmt.vof_type_expr_show_all x));
*)
  let rec aux x = 
    match (Types.Transient_expr.repr x).desc with
    | Types.Tconstr(path, _xs, _aref) -> path
    | Types.Tlink t -> aux t
    | _ ->
      (* pr2 (OCaml.string_of_v (Meta_ast_cmt.vof_type_expr_show_all x)); *)
      raise Todo
  in
  let path = aux x in
  name_of_path path

(* For Field, Constructor, subcomponent of a type. We pass
 * a lid here because the resolved open are handled by looking at texpr.
 *)
let add_use_edge_lid env (lid: Longident.t Asttypes.loc) texpr kind =
 if env.phase =*= Uses then begin
  (* get the actual field or constructor name *)
  let str = 
    (* the typename already contains the qualifier *)
    let name = name_of_longident_loc lid in
    Common2.list_last (path_resolve_locals env name kind) 
  in
  let tname = path_resolve_locals env (typename_of_texpr texpr) E.Type in
  let tname = path_type_resolve_aliases env tname in
  let full_ident = tname @ [str] in
  let node = (s_of_n full_ident, kind) in
  if G.has_node node env.g
  then add_use_edge env node (lid.Asttypes.loc)
  else begin
    (match tname with
    | ("unit" | "bool" | "list" | "option" | "exn")::_ -> ()
      (* todo: pfff specific, tofix *)
    | _ when tname |> List.exists (function 
      "LIST" | "Array_id" | "dbty" -> true | _-> false) -> ()
    | _ -> env.lookup_fail env node
    )
  end
 end

(* for identifiers of Function, Constant, etc *)
let add_use_edge_name env name loc texpr =
  if env.phase =*= Uses then begin
    let kind = kind_of_type_expr texpr in
    let name = path_resolve_locals env name kind in
    let name = path_resolve_aliases env name in
    let node = (s_of_n name, kind) in
    if G.has_node node env.g
    then add_use_edge env node loc
    else 
      (match kind with
      (* ugly: the right fix is to resolve texpr *)
      | E.Constant when G.has_node (s_of_n name, E.Function) env.g ->
          add_use_edge env (s_of_n name, E.Function) loc
      | _ ->
          if not (use_of_undefined_ok name)
          then env.lookup_fail env node
      )
  end

(* for Type *)
let add_use_edge_type env name loc = 
  if env.phase =*= Uses then begin
    let kind = E.Type in

    let name = path_resolve_locals env name E.Type in
    let name = path_type_resolve_aliases env name in
    let node = (s_of_n name, kind) in
    if G.has_node node env.g
    then add_use_edge env node loc
    else 
      if not (use_of_undefined_ok name || is_builtin_type (fst node))
      then env.lookup_fail env node
  end

(*****************************************************************************)
(* Empty wrappers *)
(*****************************************************************************)

open Graph_code_cmt_helpers

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses ~root env ast readable_cmt =

  let current =
    (* Module names are supposed to be unique for the ocaml linker,
     * but it's common to have multiple main.ml files that are linked
     * separately. But for codegraph we want to have a graph of all
     * those files, and so to avoid conflicts we use the filename
     * instead of the module. Anyway there should be no external
     * reference to those modules so we should be safe to not use
     * a E.Module.
     * less: could also mark those as a dupe module and generate a File 
     *)
    if ast.cmt_modname =~ "Main.*" || 
       ast.cmt_modname =~ "[dD]une__exe.*"
    then (readable_cmt, E.File)
    else (ast.cmt_modname, E.Module)
  in
  let env = { env with
    current;
    current_entity = [fst current];
    cmt_file = readable_cmt;
    (* we want a readable format here *)
    ml_file = readable_path_of_ast ast root readable_cmt env.source_finder;
    locals = [];
    full_path_local_value = ref [];
    full_path_local_type = ref [];
    full_path_local_module = ref [];
  }
  in
  if env.phase =*= Defs then begin
    let dir = Filename.dirname readable_cmt in
    G.create_intermediate_directories_if_not_present env.g dir;
    env.g |> G.add_node env.current;
    env.g |> G.add_edge ((dir, E.Dir), env.current) G.Has;
  end;
  if env.phase =*= Uses then begin
    ast.cmt_imports |> List.iter (fun (_s, _digest) ->
      (* old: add_use_edge env (s, E.Module)
       * actually ocaml list as dependencies many things which are not.
       * Most modules will have in their import for instance Sexp,
       * even though they don't use anything from Sexp, not sure why ...
       *)
      ()
    );
  end;
  binary_annots env ast.cmt_annots

and binary_annots env = function
  | Implementation s -> 
      structure env s
  (* sometimes we have just the .cmti, no .cmt, so we need to process those,
   * e.g. asttypes.cmti in ocaml source 
   * todo? also for core lib we actually may prefer to process the .mli?
   *)
  | Interface s ->
     signature env s

  | Packed _ 
  | Partial_implementation _ | Partial_interface _ ->
      UCommon.pr2_gen env.current;
      raise Todo

and structure env 
 { str_items = v_str_items;  str_type = _v_str_type; str_final_env = _env } =
  List.iter (structure_item env) v_str_items
and structure_item env 
 { str_desc = v_str_desc; str_loc = loc; str_env = _ } =
  structure_item_desc env loc v_str_desc

and pattern : type a. env -> a general_pattern -> unit =
 fun env -> function
  { pat_desc = v_pat_desc; pat_type = v_pat_type; 
    pat_loc = _v_pat_loc; pat_extra = _v_pat_extra; pat_env = _v_pat_env;
    pat_attributes = _;
  } ->
  pattern_desc v_pat_type env v_pat_desc

and expression env
    { exp_desc = v_exp_desc; exp_loc = _v_exp_loc;  exp_extra = __v_exp_extra;
      exp_type = v_exp_type; exp_env = _v_exp_env;
      exp_attributes = _;
    } =
  expression_desc v_exp_type env v_exp_desc
and module_expr env
    { mod_desc = v_mod_desc; mod_loc = _v_mod_loc;
      mod_type = v_mod_type; mod_env = _v_mod_env;
      mod_attributes = _;
    } =
  module_expr_desc env v_mod_desc;
  Types.module_type env v_mod_type

and signature env
  { sig_items = v_sig_items; sig_type = _v_sig_type; sig_final_env = _env } =
  List.iter (sig_item env) v_sig_items
and sig_item env
    { sig_desc = v_sig_desc; sig_env = _; sig_loc = loc } =
  sig_item_desc env loc v_sig_desc

(* ---------------------------------------------------------------------- *)
(* Structure *)
(* ---------------------------------------------------------------------- *)
and structure_item_desc env loc = function
  | Tstr_eval (v1, _attrs) -> 
    let full_ident = env.current_entity @ ["__toplevel__"] in
    let node = (full_ident, E.TopStmts) in
    let env = add_node_and_edge_if_defs_mode ~dupe_ok:true env node loc in
    expression env v1

  | Tstr_value ((rec_flag, xs)) ->
      (* first pass *)
      if rec_flag =*= Asttypes.Recursive then begin
        xs |> List.iter (fun vb ->
          let pat = vb.vb_pat in
          let exp = vb.vb_expr in
          match pat.pat_desc with
          | Tpat_var(id, _loc) | Tpat_alias (_, id, _loc) ->
              let full_ident = env.current_entity @ [string_of_id id] in
              add_full_path_local env (string_of_id id, full_ident) 
                (kind_of_type_expr exp.exp_type)
          | _ -> ()
        );
      end;

      (* second pass *)
      xs |> List.iter (fun vb ->
        let pat = vb.vb_pat in
        let exp = vb.vb_expr in
        match pat.pat_desc with
        | Tpat_var(id, loc) | Tpat_alias (_, id, loc) ->
            let full_ident = env.current_entity @ [string_of_id id] in
            let node = (full_ident, kind_of_type_expr exp.exp_type) in
            (* some people do let foo = ... let foo = ... in the same file *)
            let env = add_node_and_edge_if_defs_mode ~dupe_ok:true env node 
              (unwrap loc) in
            expression env exp
(* TODO
        | Tpat_construct(p, loc, _ctor, [], false) when name_of_path p = ["()"]->
          let full_ident = env.current_entity @ ["__toplevel__"] in
          let node = (full_ident, E.TopStmts) in
          let env = 
            add_node_and_edge_if_defs_mode ~dupe_ok:true env node (unwrap loc)in
          expression env v2
*)
        | Tpat_tuple xs ->
            let xdone = ref false in
            xs |> List.iter (fun p ->
              match p.pat_desc with
              | Tpat_var(id, loc) | Tpat_alias (_, id, loc) ->
                  let full_ident = env.current_entity @ [string_of_id id] in
                  let node = (full_ident, kind_of_type_expr p.pat_type) in
                  let env = 
                    add_node_and_edge_if_defs_mode ~dupe_ok:true env node 
                      (unwrap loc) in

                  (* arbitrarily choose the first one as the source for v2 *)
                  if not !xdone then begin
                    xdone := true;
                    expression env exp
                  end
              | _ -> 
                  pattern env p
            );
            if not !xdone then expression env exp
      
        | _ ->
            let env = {env with locals = env.locals } in
            pattern env pat;
            expression env exp;
      )
  | Tstr_primitive vd ->
      let id = vd.val_id in
      let loc = vd.val_loc in

      let full_ident = env.current_entity @ [string_of_id id] in
      let node = (full_ident, kind_of_value_descr vd) in
      let env = add_node_and_edge_if_defs_mode env node loc in
      value_description env vd
  | Tstr_type (_rec, xs) ->
      (* todo? should do different things depending on rec flag? *)
      (* first pass *)
      xs |> List.iter (fun td ->
        let id = td.typ_id in
        let full_ident = env.current_entity @ [string_of_id id] in
        add_full_path_local env (string_of_id id, full_ident) E.Type
      );
      (* second pass *)
      xs |> List.iter (fun td -> 
        let id = td.typ_id in
        let loc = td.typ_loc in
        let full_ident = env.current_entity @ [string_of_id id] in
        let node = (full_ident, E.Type) in
        let env = add_node_and_edge_if_defs_mode env node (loc) in

        (match td.typ_kind, td.typ_manifest with
        | Ttype_abstract, Some ({ctyp_desc=Ttyp_constr (path, _lid, _xs); _}) ->
          if env.phase =*= Defs then
            let name = name_of_path path in
            Stack_.push (full_ident, path_resolve_locals env name E.Type)
              env.type_aliases
        | _ -> ()
        );
        type_declaration env td
      )
  | Tstr_exception ec ->
      let ec = ec.tyexn_constructor in
      let id = ec.ext_id in
      let loc = ec.ext_loc in
      let v3  = ec.ext_type in

      let full_ident = env.current_entity @ ["exn";string_of_id id] in
      let node = (full_ident, E.Exception) in
      let env = 
        add_node_and_edge_if_defs_mode ~dupe_ok:true env node (loc) in
      exception_declaration env v3
(*
  | Tstr_exn_rebind ((id, loc, v3, _loc2)) ->
      let full_ident = env.current_entity @ ["exn";string_of_id id] in
      let node = (full_ident, E.Exception) in
      let env = add_node_and_edge_if_defs_mode env node (unwrap loc) in
      path_t env v3
*)
  | Tstr_module mb  ->
      let id = mb.mb_id in
      let loc = mb.mb_loc in
      let modexpr = mb.mb_expr in

      let full_ident = env.current_entity @ [string_of_id_opt id] in
      let node = (full_ident, E.Module) in
      (match modexpr.mod_desc with
      | Tmod_ident (path, _lid) ->
          (* do not add nodes for module aliases in the graph *)
          if env.phase =*= Defs then begin
            let name = name_of_path path in
            Stack_.push (full_ident, path_resolve_locals env name E.Module) 
              env.module_aliases
          end;
          add_full_path_local env (string_of_id_opt id, full_ident) E.Module
      | _ -> 
          let env =
            (* since ocaml 4.07 the stdlib has been reorganized with
             * pervasives.ml becoming stdlib.ml with a nested
             * module Pervasives = struct ... which we do not create
             * here because calls to pervasives entities are transformed
             * by the compiler in Stdlib.xxx, not Stdlib.Pervasives.xxx
             *)
            if string_of_id_opt id = "Pervasives" 
            then env
            else add_node_and_edge_if_defs_mode env node (loc) 
          in
          module_expr env modexpr
      )
  | Tstr_recmodule xs ->
      List.iter (fun { mb_id = id; mb_loc = loc; mb_expr = me; _ } ->
        let full_ident = env.current_entity @ [string_of_id_opt id] in
        let node = (full_ident, E.Module) in
        let env = add_node_and_edge_if_defs_mode env node (loc) in
        (* module_type env v3; *)
        module_expr env me;
      ) xs
  | Tstr_modtype mtd  ->
    let id = mtd.mtd_id in
    let typ_opt = mtd.mtd_type in

    let _ = Ident.t env id
    and _ = v_option (module_type env) typ_opt
    in ()

  (* opened names are resolved, no need to handle that I think *)
  | Tstr_open _od  ->
(* OLD: 
    let path = od.open_path in
    path_t env path
*)
    ()
  | Tstr_include _incd ->
(*
      let _ = module_expr env v1 and _ = List.iter (Ident.t env) v2 in ()
*)
    ()

  | (Tstr_class _ | Tstr_class_type _ | Tstr_typext _ |Tstr_attribute _) -> 
    (*pr2_once (spf "TODO: str_class, %s" env.file)*)
    ()

and type_declaration env
    { typ_params = __v_typ_params; typ_type = v_typ_type;
      typ_cstrs = v_typ_cstrs; typ_kind = v_typ_kind;
      typ_private = _v_typ_private; typ_manifest = v_typ_manifest;
      typ_loc = _v_typ_loc;
      typ_id = _;
      typ_name = _;
      typ_attributes = _;
    } =
  let _ = Types.type_declaration env v_typ_type in
  let _ =
    List.iter
      (fun (v1, v2, _loc) ->
         let _ = core_type env v1
         and _ = core_type env v2
         in ())
      v_typ_cstrs in
  let _ = type_kind env v_typ_kind in
  let _ = v_option (core_type env) v_typ_manifest in
  ()
and type_kind env = function
  | Ttype_abstract -> ()
  | Ttype_variant xs ->
      List.iter (fun cd  ->
        let id = cd.cd_id in
        let loc = cd.cd_loc in
        let args = cd.cd_args in
        let full_ident = env.current_entity @ [string_of_id id] in
        let node = (full_ident, E.Constructor) in
        let env = add_node_and_edge_if_defs_mode env node (loc) in
        constructor_arguments env args
      ) xs
  | Ttype_record xs ->
      List.iter (fun ld ->
        let id = ld.ld_id in
        let loc = ld.ld_loc in
        let typ = ld.ld_type in
        let full_ident = env.current_entity @ [string_of_id id] in
        let node = (full_ident, E.Field) in
        let env = add_node_and_edge_if_defs_mode env node (loc) in
        core_type env typ;
      ) xs
  | Ttype_open ->
    todo env "Ttype_open"

and constructor_arguments env = function
  | Cstr_tuple (args) ->
        List.iter (core_type env) args
   (* ext: anonymous record, since ocaml 4.06? *)
  | Cstr_record (lbls) ->
    (* less: create a _anon_ parent type like for Tstr_type? *)
    (* mostly copy paste of of Type_record above *)
     lbls |> List.iter (fun ld ->
        let id = ld.ld_id in
        let loc = ld.ld_loc in
        let typ = ld.ld_type in
        let full_ident = env.current_entity @ [string_of_id id] in
        let node = (full_ident, E.Field) in
        let env = add_node_and_edge_if_defs_mode env node (loc) in
        core_type env typ;
      )


and exception_declaration _env _x =
  ()
(* TODO
 { exn_params = v_exn_params; exn_exn = v_exn_exn; exn_loc = _v_exn_loc } =
  let _ = List.iter (core_type env) v_exn_params in
  let _ = Types.exception_declaration env v_exn_exn in
  ()
*)

(* ---------------------------------------------------------------------- *)
(* Signature *)
(* ---------------------------------------------------------------------- *)
and sig_item_desc env loc = function
  | Tsig_type (recflag, xs) -> 
    structure_item_desc env loc (Tstr_type (recflag, xs))
  | _ -> UCommon.pr2_once "TODO: sig_item_desc"

(* ---------------------------------------------------------------------- *)
(* Pattern *)
(* ---------------------------------------------------------------------- *)
and pattern_desc : type a. TypesOld.type_expr -> env -> a pattern_desc -> unit =
  fun t env -> function
  | Tpat_exception _ -> 
      todo env "Tpat_exception"
  | Tpat_value v1 -> 
      (* v1 has a weird private type that forbid to access it, so
       * use Obj.magic to go around it *)
      let v1 = Obj.magic v1 in
      pattern env v1
  | Tpat_any -> ()
  | Tpat_var ((id, _loc)) ->
      env.locals <- string_of_id id :: env.locals
  | Tpat_alias ((v1, id, _loc)) ->
      pattern env v1;
      env.locals <- string_of_id id :: env.locals
  | Tpat_constant v1 -> 
      constant env v1
  | Tpat_tuple xs -> 
      List.iter (pattern env) xs
  | Tpat_construct (lid, v3, v4, _v5)
    ->
      add_use_edge_lid env lid t E.Constructor;
      let _ = constructor_description env v3
      and _ = List.iter (pattern env) v4
      in ()
  | Tpat_variant ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = v_option (pattern env) v2
      and _ = v_ref (row_desc env) v3
      in ()
  | Tpat_record ((xs, _closed_flag)) ->
      List.iter (fun 
        (lid, _v2, v3) 
      ->
        add_use_edge_lid env lid t E.Field;
        let _ = label_description env v3
        and _ = pattern env v3
        in ()
      ) xs
  | Tpat_array xs -> 
      List.iter (pattern env) xs
  | Tpat_or ((v1, v2, v3)) ->
      let _ = pattern env v1
      and _ = pattern env v2
      and _ = v_option (row_desc env) v3
      in ()
  | Tpat_lazy v1 -> 
      pattern env v1

and value_case env = 
   fun { c_lhs = pat; c_rhs = exp; c_guard = guard } ->
          let _ = pattern env pat
          and _ = v_option (expression env) guard
          and _ = expression env exp in ()

(* copy-paste of above; alt: use a 'a. type annotation quantification *)
and computation_case env = 
   fun { c_lhs = pat; c_rhs = exp; c_guard = guard } ->
          let _ = pattern env pat
          and _ = v_option (expression env) guard
          and _ = expression env exp in ()

and binding_op env { bop_exp; _ (* TODO *) } =
  expression env bop_exp

(* ---------------------------------------------------------------------- *)
(* Expression *)
(* ---------------------------------------------------------------------- *)
and expression_desc t env =
  function
  | Texp_ident (path, lid, _vd) ->
      let name = name_of_path path in
      let str = s_of_n name in
      if List.mem str env.locals
      then ()
      else add_use_edge_name env name lid.Asttypes.loc t
  | Texp_constant v1 -> 
      constant env v1
  | Texp_let ((rec_flag, xs, v3)) ->
      (* first pass *)
      if rec_flag =*= Asttypes.Recursive then begin
        xs |> List.iter (fun { vb_pat = pat; vb_expr = _exp; _ } ->
          match pat.pat_desc with
          | Tpat_var (id, _loc) | Tpat_alias (_, id, _loc) ->
              env.locals <- string_of_id id:: env.locals
          | _ -> ()
        );
      end;
      (* second pass *)
      xs |> List.iter (fun { vb_pat = pat; vb_expr = exp; _ } ->
        pattern env pat;
        expression env exp;
      );
      expression env v3

  (* just treat that like a classic let? *)
  | Texp_letop {let_; ands; param = _; body; partial = _} ->
      binding_op env let_;
      List.iter (binding_op env) ands;
      value_case env body

  | Texp_function ({ arg_label = v1; cases = v2; partial = v3; param = _TODO}) ->
      let _ = label env v1
      and _ = List.iter (value_case env) v2
      and _ = partial env v3
      in ()
  | Texp_apply ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        List.iter
          (fun (v1, v2) ->
             let _ = label env v1
             and _ = v_option (expression env) v2
             in ())
          v2
      in ()
  | Texp_match ((v1, v2, pa)) ->
      let _ = expression env v1
      and _ = List.iter (computation_case env) v2
      and _ = partial env pa
      in ()
  | Texp_try ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        List.iter (value_case env)
          v2
      in ()
  | Texp_tuple v1 -> let _ = List.iter (expression env) v1 in ()
  | Texp_construct (lid, v2, v3) 
    ->
      add_use_edge_lid env lid t E.Constructor;
      constructor_description env v2;
      List.iter (expression env) v3;

  | Texp_variant ((v1, v2)) ->
      let _ = label env v1 and _ = v_option (expression env) v2 in ()
  | Texp_record ({ fields = v1; extended_expression = v2; representation = _}) ->
      Array.iter (fun (lbl_descr, rcrd_lbl_def) ->
        match rcrd_lbl_def with
        | Overridden (lid, e) -> 
          add_use_edge_lid env lid t E.Field;
          path_t env lid;
          let _ = label_description env lbl_descr
          and _ = expression env e
          in ()
          (* less: in 4.07 { e with l1 = ...} is expanded and the
           * kept fields are stored in the typed tree too.
           *)
        | Kept _ ->
          ()
      ) v1;
      v_option (expression env) v2
  | Texp_field ((v1, lid, v2)) 
    ->
      expression env v1;
      add_use_edge_lid env lid v1.exp_type E.Field;
      label_description env v2

  | Texp_setfield ((v1, lid, v3, v4)) 
    ->
      expression env v1;
      add_use_edge_lid env lid v1.exp_type E.Field;
      label_description env v3;
      expression env v4;

  | Texp_array xs -> 
      List.iter (expression env) xs

  | Texp_ifthenelse ((v1, v2, v3)) ->
      let _ = expression env v1
      and _ = expression env v2
      and _ = v_option (expression env) v3
      in ()
  | Texp_sequence ((v1, v2)) ->
      let _ = expression env v1 and _ = expression env v2 in ()
  | Texp_while ((v1, v2)) ->
      let _ = expression env v1 and _ = expression env v2 in ()
  | Texp_for ((id, _loc_string, v3, v4, _direction_flag, v6)) ->
      expression env v3;
      expression env v4;
      let env = { env with locals = string_of_id id::env.locals } in
      expression env v6
  | Texp_send ((v1, v2)) ->
      let _ = expression env v1
      and _ = meth env v2
      in ()
  | Texp_new ((v1, _loc_longident, v3)) ->
      let _ = path_t env v1
      and _ = Types.class_declaration env v3
      in ()
  | Texp_instvar ((v1, v2, _loc)) ->
      let _ = path_t env v1
      and _ = path_t env v2
      in ()
  | Texp_setinstvar ((v1, v2, _loc, v4)) ->
      let _ = path_t env v1
      and _ = path_t env v2
      and _ = expression env v4
      in ()
  | Texp_override ((v1, v2)) ->
      let _ = path_t env v1
      and _ =
        List.iter
          (fun (v1, _loc, v3) ->
             let _ = path_t env v1
             and _ = expression env v3
             in ())
          v2
      in ()
  | Texp_letmodule ((v1, _loc, _vpresence, v3, v4)) ->
      let _ = Ident.t env v1
      and _ = module_expr env v3
      and _ = expression env v4
      in ()
  | Texp_assert v1 -> let _ = expression env v1 in ()
  | Texp_lazy v1 -> let _ = expression env v1 in ()
  | Texp_object ((v1, v2)) ->
      let _ = class_structure env v1 and _ = List.iter v_string v2 in ()
  | Texp_pack v1 -> let _ = module_expr env v1 in ()
  | Texp_unreachable ->
      todo env "Texp_unreachable"
  | Texp_letexception (_exn_ctorTODO, e) ->
     (* todo: in theory we should define locally this exn *)
      expression env e
  | Texp_extension_constructor (_, _) ->
      todo env "Texp_extension_constructor"
  | Texp_open (_v1TODO, v2) -> 
      expression env v2


(*
and exp_extra env = function
  | Texp_constraint ((v1, v2)) ->
      let _ = v_option (core_type env) v1
      and _ = v_option (core_type env) v2
      in ()
  | Texp_open (_override, path, lid, _env) 
   ->
      path_t env path
  | Texp_poly v1 -> let _ = v_option (core_type env) v1 in ()
  | Texp_newtype v1 -> let _ = v_string v1 in ()
*)

(* ---------------------------------------------------------------------- *)
(* Module *)
(* ---------------------------------------------------------------------- *)
and functor_parameter env = function
  | Unit -> ()
  | Named (v1, _loc, v3) ->
      let _ = Ident.t env v1
      and _ = module_type env v3
      in ()

and module_expr_desc env =
  function
  | Tmod_ident ((v1, _loc_longident)) ->
      path_t env v1
  | Tmod_structure v1 -> structure env v1
  | Tmod_functor ((v1, v2)) ->
      let _ = functor_parameter env v1
      and _ = module_expr env v2
      in ()
  | Tmod_apply ((v1, v2, v3)) ->
      let _ = module_expr env v1
      and _ = module_expr env v2
      and _ = module_coercion env v3
      in ()
  | Tmod_constraint ((v1, v2, v3, v4)) ->
      let _ = module_expr env v1
      and _ = Types.module_type env v2
      and _ = module_type_constraint env v3
      and _ = module_coercion env v4
      in ()
  | Tmod_unpack ((v1, v2)) ->
      let _ = expression env v1 
      and _ = Types.module_type env v2 
      in ()
(* ---------------------------------------------------------------------- *)
(* Type *)
(* ---------------------------------------------------------------------- *)
and core_type env
    { ctyp_desc = v_ctyp_desc; ctyp_type = __v_ctyp_type;
      ctyp_env = _v_ctyp_env; ctyp_loc = _v_ctyp_loc;
      ctyp_attributes = _;
    } =
  core_type_desc env v_ctyp_desc
and core_type_desc env =
  function
  | Ttyp_any -> ()
  | Ttyp_var v1 -> let _ = v_string v1 in ()
  | Ttyp_arrow ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = core_type env v2
      and _ = core_type env v3
      in ()
  | Ttyp_tuple v1 -> let _ = List.iter (core_type env) v1 in ()
  | Ttyp_constr ((path, lid, v3)) ->
      let name = name_of_path path in
      add_use_edge_type env name lid.Asttypes.loc;
      let _ = path_t env path
      and _ = List.iter (core_type env) v3
      in ()
  | Ttyp_object (v1, _closed) -> 
    let _ = List.iter (object_field env) v1
    in ()
  | Ttyp_class ((v1, _loc_longident, v3)) ->
      let _ = path_t env v1
      and _ = List.iter (core_type env) v3
      in ()
  | Ttyp_alias ((v1, v2)) ->
      let _ = core_type env v1 and _ = v_string v2 in ()
  | Ttyp_variant ((v1, _bool, v3)) ->
      let _ = List.iter (row_field env) v1
      and _ = v_option (List.iter (label env)) v3
      in ()
  | Ttyp_poly ((v1, v2)) ->
      let _ = List.iter v_string v1 and _ = core_type env v2 in ()
  | Ttyp_package _v1 -> 
    UCommon.pr2_once (spf "TODO: Ttyp_package, %s" env.cmt_file)

and object_field env x : unit = 
  match x.of_desc with
  | OTtag (_str, typ) -> 
      core_type env typ
  | OTinherit _ ->
    todo env "OTinherit"


 
and row_field env x =
  match x.rf_desc with
  | Ttag ((v1, _bool, v3)) ->
      let _ = label env v1
      and _ = List.iter (core_type env) v3
      in ()
  | Tinherit v1 -> let _ = core_type env v1 in ()
and
  value_description env
                    {
                      val_desc = v_val_desc;
                      val_val = v_val_val;
                      val_prim = v_val_prim;
                      val_loc = _v_val_loc;
                      val_id = _;
                      val_name = _;
                      val_attributes = _;
                    } =
  let _ = core_type env v_val_desc in
  let _ = Types.value_description env v_val_val in
  let _ = List.iter v_string v_val_prim in
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build (root : Fpath.t) ~cmt_files ~ml_files =
  let root = !!root in

  let files = cmt_files in
  let g = G.create () in
  G.create_initial_hierarchy g;

  (* use Hashtbl.find_all property *)
  let hstat_lookup_failures = Hashtbl.create 101 in

  let env = { 
    g;
    module_aliases = ref [];
    type_aliases = ref [];
    phase = Defs;
    cmt_file = "filled_later";
    ml_file = "filled later";
    source_finder = 
      Graph_code.basename_to_readable_disambiguator ~root ml_files;
    current = ("filled_later", E.File);
    current_entity = [];
    locals = [];
    full_path_local_type = ref [];
    full_path_local_value = ref [];
    full_path_local_module = ref [];
    lookup_fail = (fun env dst ->
      let src = env.current in
      Logs.warn (fun m -> m "PB: lookup_fail on %s (in %s, in file %s)"
             (G.string_of_node dst) (G.string_of_node src) env.cmt_file);
      (* less: could also use Hashtbl.replace to count entities only once *)
      Hashtbl.add hstat_lookup_failures dst true;
    );
  } in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  Logs.info (fun m -> m "STEP1: extract defs");
  files |> List.iter (fun file ->
      let ast = parse file in
      let readable_cmt = undo_dune_cmtfile (Filename_.readable ~root file) in
      try 
      extract_defs_uses ~root { env with phase = Defs } ast readable_cmt
      with 
      (* let main_codegraph_build catch it and print a nice error *)
      | (Graph_code.Error err) as _exn -> 
        failwith (spf "graph error with %s (exn = %s)" 
         file (Graph_code.string_of_error err))
      | exn ->
        failwith (spf "exn with %s (exn = %s)" file (Common.exn_to_s exn))
    );

  (* step2: creating the 'Use' edges *)
  Logs.info (fun m -> m "STEP2: extract uses");
  files |> List.iter (fun file ->
      Logs.info (fun m -> m "analyzing %s" file);
      let ast = parse file in
      let readable_cmt = undo_dune_cmtfile (Filename_.readable ~root file) in
      (* old: pad: a bit pad specific *)
      if readable_cmt =~ "^external"
      then ()
      else extract_defs_uses ~root { env with phase = Uses} ast readable_cmt
    );
  if !debug then begin
    UCommon.pr2 "";
    UCommon.pr2 "Module aliases";
    !(env.module_aliases) |> List.iter UCommon.pr2_gen;
    UCommon.pr2 "Type aliases";
    !(env.type_aliases) |> List.iter UCommon.pr2_gen;
  end;

  (* lookup failures summary *)
  let xs = Hashtbl_.hashset_to_list hstat_lookup_failures in
  let modules = xs |> List.filter_map
    (fun node-> 
        try 
          Some (top_module_of_node node, ())
        with Failure _ ->
            (* todo: ex:
             * could not find top module of Function: sexp_of_compilation_unit
             *)
            None
    ) in
  let counts = modules |> Assoc.group_assoc_bykey_eff 
                       |> List.map (fun (x, xs)-> x, List.length xs) 
                       |> Assoc.sort_by_val_highfirst
                       |> List_.take_safe 20
  in
  UCommon.pr2 "Top lookup failures per modules";
  counts |> List.iter (fun (s, n) -> UCommon.pr2 (spf "%-30s = %d" s n));

  (* finally return the graph *)
  g
