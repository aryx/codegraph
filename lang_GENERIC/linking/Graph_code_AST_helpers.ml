(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 *)
open Common
open Graph_code_AST_env
open AST_generic
module E = Entity_code
module G = Graph_code
module AST = AST_generic
module T = Resolved_type
module N = Resolved_name
module Stat = Stat_code

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Debugging helpers *)
(*****************************************************************************)
let string_of_any any =
  let v = Meta_AST.vof_any any in
  let s = OCaml.string_of_v v in
  s

(*****************************************************************************)
(* Stat helpers *)
(*****************************************************************************)
let wrap_stat env categ opt =
  let b = match opt with Some _ -> true | None -> false in
  Stat.add env.stats categ b;
  opt

(*****************************************************************************)
(* Defs/Uses control *)
(*****************************************************************************)
let when_defs_phase env f = if env.phase =*= Defs then f () else ()

let when_uses_phase env f = if env.phase =*= Uses then f () else ()

let when_uses_phase_or_none env f = if env.phase =*= Uses then f () else None

(*****************************************************************************)
(* Name helpers *)
(*****************************************************************************)

(* This is now used only in xxx-semgrep/.../Run.ml
 * less: move it there?
 *)
let rec dotted_ident_of_exprkind_opt ekind =
  match ekind with
  | N name -> N.of_ast_name name
  | DotAccess (v1, _v2, v3) -> (
      let* xs = dotted_ident_of_exprkind_opt v1.e in
      match v3 with
      (* TODO? we could potentially set idinfo.resolved here *)
      | FN (Id (id, _idinfo)) -> Some (xs @ [ id ])
      | _ -> None)
  | _ -> None

(* less: could be moved in AST_generic_helpers *)
let info_of_name n =
  match n with
  | Id (id, _) | IdQualified { name_last = (id, _); _ } -> snd id

(*****************************************************************************)
(* Entity helpers *)
(*****************************************************************************)

(* For now we handle only sample entities like function/class definitions
 * where the name is a simple identifier.
 *)
let ident_of_entity_opt _env ent =
  match ent.name with
  | EN (Id (id, _)) -> Some id
  | _ -> None

let entity_kind_of_definition _env (ent, defkind) =
  match defkind with
  | FuncDef _ -> E.Function (* less: could be also Method *)
  | ClassDef _ -> E.Class
  (* TODO: look parent node, if already in a function, then it's a local? *)
  | VarDef _ ->
      if
        AST_generic_helpers.has_keyword_attr AST.Final ent.attrs
        || AST_generic_helpers.has_keyword_attr AST.Const ent.attrs
      then E.Constant
      else E.Global
  | _ ->
      logger#error "entity kind not handled yet: %s"
        (string_of_any (Dk defkind));
      E.Other "Todo"

(*****************************************************************************)
(* Typing helpers *)
(*****************************************************************************)

let type_of_module_opt env entname =
  if env.lang =*= Python then
    (* This is to allow to treat Python modules like classes
     * where you can do mod.function like for a field access.
     * The type of the module is then simply its name,
     * which will allow lookup_type_of_dotted_ident_opt to work.
     *)
    let tk =
      Parse_info.first_loc_of_file env.readable |> Parse_info.mk_info_of_loc
    in
    let xs = N.dotted_ident_of_entname entname in
    Some (T.TN (xs |> List.map (fun s -> (s, tk))))
  else None

let type_of_definition_opt env dotted_ident (_ent, defkind) =
  match defkind with
  (* for a class, its name is his type *)
  | ClassDef _ -> Some (T.TN dotted_ident)
  | VarDef { vtype = Some ty; _ } -> 
      T.of_ast_type env.file_or_package_qualifier ty
  | FuncDef { frettype = Some ty; fparams; _ } ->
      let* ty = T.of_ast_type env.file_or_package_qualifier ty in
      Some (T.TFunction (fparams, ty))
  | _ -> None

(*****************************************************************************)
(* Graph builders helpers *)
(*****************************************************************************)

(* quite similar to create_intermediate_directories_if_not_present, but
 * for Packages.
 * java-specific?
 *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common2.inits xs |> List.map (fun xs -> Common.join "." xs) in
  let dirs =
    match dirs with
    | "" :: xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x :: xs ->
        let entity = (x, E.Package) in
        if G.has_node entity g then aux entity xs
        else (
          g |> G.add_node entity;
          g |> G.add_edge (current, entity) G.Has;
          aux entity xs)
  in
  aux root dirs

(* Useful in Test.ml to intercept the position of all use edges.
 * alt: we could extend G.edgeinfo to contain a position
*)
let (hook_add_use_edge : (G.node -> G.node -> Parse_info.t -> unit) ref) = 
  ref (fun _ _ _ -> ())

let add_use_edge env (name, kind) tok =
  let src = env.current_parent in
  let dst = (name, kind) in
  logger#info "trying %s --> %s" (G.string_of_node src) (G.string_of_node dst);
  match () with
  | _ when not (G.has_node src env.g) ->
      logger#error "LOOKUP SRC FAIL %s --> %s, src does not exist???"
        (G.string_of_node src) (G.string_of_node dst)
  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g;
      !hook_add_use_edge src dst tok;
  | _ ->
     let kind_original = kind in
     let dst = (name, kind_original) in
     let parent_target = G.not_found in
     (match kind_original with
     | E.Package ->
         let fake_package =
           Common.split "\\." name |> List.map (fun s -> s ^ "2")
         in
         let dst = (Common.join "." fake_package, kind_original) in
         if not (G.has_node dst env.g) then
           (* disabled for now
              create_intermediate_packages_if_not_present
                env.g parent_target
                (fake_package |> List.map (fun s -> s,()));
           *)
           logger#error "PB: lookup fail on %s (in %s)"
             (G.string_of_node dst) (G.string_of_node src);
         env.g |> G.add_edge (src, dst) G.Use;
         !hook_add_use_edge src dst tok;
     | _ ->
         pr2
           (spf "PB: lookup fail on %s (in %s)" (G.string_of_node dst)
              (G.string_of_node src));
         G.add_node dst env.g;
         env.g |> G.add_edge (parent_target, dst) G.Has;
         env.g |> G.add_edge (src, dst) G.Use;
         !hook_add_use_edge src dst tok;
      )  

let nodeinfo_of_id env (_id, tk) =
  let loc = Parse_info.unsafe_token_location_of_info tk in
  let loc = { loc with Parse_info.file = env.readable } in
  { G.pos = loc; props = []; typ = None }

let nodeinfo_of_file readable =
  let loc = Parse_info.first_loc_of_file readable in
  { G.pos = loc; props = []; typ = None }

let (hook_add_nodeinfo: (G.node -> G.nodeinfo -> unit) ref) = 
  ref (fun _ _ -> ())

let add_nodeinfo env node id =
  let info = nodeinfo_of_id env id in
  env.g |> G.add_nodeinfo node info;
  !hook_add_nodeinfo node info
