(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
module Flag = Flag_parsing
module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Obsolete file: see graph_code_cmt.ml for a more complete graph.
 *
 * Partial graph of dependencies for OCaml using essentially just the
 * open directives.
 *
 * todo? if give edges a weight, then we need to modulate it depending on
 * the type of the reference. Two references to a function in another
 * module is more important than 10 references to some constructors?
 * If we do some pattern matching on 20 constructors, is it more
 * important than two functions calls?
 * So Type|Exception > Function|Class|Global >> Constructors|constants ?
 *
 * notes:
 *  - ml vs mli? just get rid of mli? but one can also want to
 *    care only about mli dependencies, like I did with my 'make doti'.
 *    We can introduce a Module entity that is the parent of the
 *    ml and mli file (this has-graph unify many things :) ).
 *
 *    TODO but there is still the issue about where to put the edge
 *    when one module call a function in another module. Do we
 *    link the call to the def in the mli or in the ml?
 *
 * schema:
 *  Root -> Dir -> Module -> File (.ml) -> # TODO
 *
 *                        -> File (.mli)
 *       -> Dir -> File  # no intermediate Module node when there is a dupe
 *                       # on a module name (e.g. for main.ml)
 *
 *       -> Dir -> SubDir -> Module -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file =
  Common.save_excursion Flag.show_parsing_error false (fun () ->
      Common.save_excursion Flag.exn_when_lexical_error true (fun () ->
          try Parse_ml.parse_program (Fpath.v file) with
          | Parsing_error.Syntax_error _ ->
              UCommon.pr2 ("PARSING problem in: " ^ file);
              []))

(* todo: move this code in another file? module_analysis_ml.ml ? *)
let lookup_module_name h_module_aliases s =
  try Hashtbl.find h_module_aliases s with
  | Not_found -> s

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

(*
 * We just create the Dir, File, and Module entities.
 * See graph_code_cmt.ml if you want Function, Type, etc.
 *)
let extract_defs ~g ~duplicate_modules ~ast ~readable ~file =
  ignore ast;
  let dir = Filename.dirname !!readable in
  G.create_intermediate_directories_if_not_present g dir;
  let m = Module_ml.module_name_of_filename file in

  g |> G.add_node (!!readable, E.File);

  match () with
  | _ when List.mem m (Common2_.keys duplicate_modules) -> (
      (* we could attach to two parents when we are almost sure that
       * nobody will reference this module (e.g. because it's an
       * entry point), but then all the uses in those files would
       * propagate to two parents, so when we have a dupe, we
       * don't create the intermediate Module node. If it's referenced
       * somewhere then it will generate a lookup failure.
       * So just Dir -> File here, no Dir -> Module -> File.
       *)
      g |> G.add_edge ((dir, E.Dir), (!!readable, E.File)) G.Has;
      match m with
      | s when s =~ "Main.*" || s =~ "Demo.*" || s =~ "Test.*" || s =~ "Foo.*"
        ->
          ()
      | _ when file =~ ".*external/" -> ()
      | _ ->
          UCommon.pr2
            (spf "PB: module %s is already present (%s)" m
               (Dumper.dump (List.assoc m duplicate_modules))))
  | _ when G.has_node (m, E.Module) g -> (
      match G.parents (m, E.Module) g with
      (* probably because processed .mli or .ml before which created the node *)
      | [ p ] when p =*= (dir, E.Dir) ->
          g |> G.add_edge ((m, E.Module), (!!readable, E.File)) G.Has
      | x ->
          UCommon.pr2 "multiple parents or no parents or wrong dir";
          UCommon.pr2_gen (x, dir, m);
          raise Impossible)
  | _ ->
      (* Dir -> Module -> File *)
      g |> G.add_node (m, E.Module);
      g |> G.add_edge ((dir, E.Dir), (m, E.Module)) G.Has;
      g |> G.add_edge ((m, E.Module), (!!readable, E.File)) G.Has

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)
let extract_uses ~g ~ast ~readable ~dupes =
  let src = (!!readable, E.File) in

  (* when do module A = Foo, A.foo is actually a reference to Foo.foo *)
  let h_module_aliases = Hashtbl.create 101 in

  let _add_edge_if_existing_module s =
    let s = lookup_module_name h_module_aliases s in

    let target = (s, E.Module) in
    if G.has_node target g then g |> G.add_edge (src, target) G.Use
    else (
      g |> G.add_node target;
      let parent_target = if List.mem s dupes then G.dupe else G.not_found in
      g |> G.add_edge (parent_target, target) G.Has;
      g |> G.add_edge (src, target) G.Use;
      UCommon.pr2 (spf "PB: lookup fail on module %s in %s" (fst target) !!readable))
  in

  (* TODO: use Visitor_AST.ml from ml_to_generic?
     let visitor = V.mk_visitor { V.default_visitor with
      (* todo? does it cover all use cases of modules ? maybe need
       * to introduce a kmodule_name_ref helper in the visitor
       * that does that for us.
       * todo: if want to give more information on edges, need
       * to intercept the module name reference at a upper level
       * like in FunCallSimple. C-s for long_name in ast_ml.ml
       *)
      V.kitem = (fun (k, _) x ->
        (match x with
        | Open (_tok, (_qu, (Name (s,_)))) ->
            add_edge_if_existing_module s

        | Module (_, Name (s,_), _, (ModuleName ([], Name (s2,__)))) ->
            Hashtbl.add h_module_aliases s s2;
        | _ -> ()
        );
        k x
      );

      V.kqualifier = (fun (k,_) qu ->
        (match qu with
        | [] -> ()
        | (Name (s, _), _tok)::_rest ->
            add_edge_if_existing_module s
        );
        k qu
      );
     } in
     visitor (Program ast);
  *)
  ignore ast;
  let _ = raise Todo in
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose = true) (root : Fpath.t) files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let duplicate_modules =
    files
    |> Assoc.group_by_mapped_key (fun f -> Filename.basename f)
    |> List.filter (fun (_k, xs) -> List.length xs >= 2)
    |> List.map (fun (k, xs) -> (Module_ml.module_name_of_filename k, xs))
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then UCommon.pr2 "\nstep1: extract defs";
  files |> List.iter (fun file ->
             let readable = Filename_.readable ~root (Fpath.v file) in
             let ast = (* parse file *) () in
             extract_defs ~g ~duplicate_modules ~ast ~readable ~file);

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then UCommon.pr2 "\nstep2: extract uses";
  files |> List.iter (fun file ->
             let readable = Filename_.readable ~root (Fpath.v file) in
             (* skip files under external/ for now *)
             if !!readable =~ ".*external/" || !!readable =~ "web/.*" then ()
             else
               let ast = parse file in
               extract_uses ~g ~ast ~readable
                 ~dupes:(List.map fst duplicate_modules));

  g
