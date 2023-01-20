(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 *)
open Common
module E = Entity_code
module H = Graph_code_AST_helpers
module L = Graph_code_AST_lookup
module T = Resolved_type
module N = Resolved_name
module Lang_specific = Graph_code_AST_lang_specific
module Stat = Stat_code

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Graph of dependencies for the generic AST.
 *
 * See graph_code.ml (in pfff), AST_generic.ml (in semgrep), and
 * main_codegraph.ml (in codegraph) for more information.
 *
 * On the file location of Graph_code_AST.ml:
 * This file is now in the deep-semgrep repository. That prevents me to use it
 * in other OSS projects like codegraph (especially codegraph_build) or
 * codecheck. But actually, codegraph was designed to work with a generated
 * graph_code.marshall and was designed to be independent of programming
 * languages. Same with codecheck. So this is ok.
 * That prevents me to use it for the _.pyi checker that I want to use 
 * for semgrep. However, we could release pycheck binaries and use that
 * in semgrep (anyway, it's not really used by the semgrep-Python team
 * for now and may never be), so not a big deal there too then.
 *
 * Graph_code_AST.ml leverages Naming_AST.ml and generalizes it
 * to resolve names globally, so it could sense to have them close together.
 * It would be also closer to AST_generic.ml. But we want deep-semgrep
 * to remain proprietary for now, so better move it out of semgrep.
 *
 * Note that even though Graph_code_AST.ml is inside the deep-semgrep
 * repository, its library name is pfff-lang_GENERIC-linking and does
 * not have dependencies to other semgrep (or deep-semgrep) modules, so we
 * could easily extract it from deep-semgrep at some point and move it in
 * codegraph for example.
 *
 * What should we do in Naming_AST vs Graph_code_AST? Should we copy
 * and generalize all the Naming_AST logic in Graph_code_AST? It
 * could be nice to have all the resolving code in one file. However,
 * we want to do some name resolving in semgrep too, and local typing
 * there too, so it is actually better to do all what can be done intra-file
 * in Naming_AST.ml and only do the interfile "linking" in Graph_code_AST.ml.
 * We actually can reuse lots of the work done in Naming_AST.ml like
 * the id_resolved import/use, but also id_type! That way we don't have
 * to handle locals/parameters here. This was handled already before and
 * focus only on the imported/exported entities.
 *
 * related work:
 *  - stackgraph (and scope graph) by github
 *  - LSP server
 *  - Datalog?
 *
 * TODO:
 *  - TODO handle the import foo.bar.* here, which could not be handled
 *    in Naming_AST.ml
 *  - SEMI integrate graph_code_java.ml
 *  - integrate graph_code_cmt.ml
 *  - integrate the other language-specific graph_code_xxx.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
open Graph_code_AST_env

(* TODO: does not parse well with pfff
type hooks = Graph_code_AST_env.hooks = {
  on_def: (Graph_code.node * AST_generic.definition) -> unit;
}
*)

let default_hooks : hooks =
  { on_def_node = (fun _ _ -> ()); on_extend_edge = (fun _ _ _ -> ()) }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* See Graph_code_AST_helpers *)

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)
(* see Graph_code_AST_lookup *)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
(* see Graph_code_AST_visitor *)

let extract_defs_uses env ast =
  (if env.phase =*= Defs then
   match env.current_parent with
   | base, E.File ->
       let dir = Common2.dirname env.readable in
       G.create_intermediate_directories_if_not_present env.g dir;
       let node = (base, E.File) in
       env.g |> G.add_node node;
       env.g |> G.add_nodeinfo node (H.nodeinfo_of_file env.readable);
       env.g |> G.add_edge ((dir, E.Dir), (base, E.File)) G.Has;
       (* this is mostly for Python where files act also as class/module
        * where you can also use the dot notation, but that means the name
        * of the module is an entity that must contain a type for
        * L.lookup_type_of_dotted_ident_opt to work
        *)
       H.type_of_module_opt env base
       |> Option.iter (fun ty ->
              logger#info "adding type for %s = %s" (G.string_of_node node)
                (T.show ty);
              Hashtbl.add env.types node ty)
   | entname, E.Package ->
       let xs = N.dotted_ident_of_entname entname in
       H.create_intermediate_packages_if_not_present env.g G.root xs
   (* flat namespace, used temporarily for Ruby right now *)
   | ".", E.Dir -> ()
   | n -> failwith (spf "top parent not handled yet: %s" (G.string_of_node n)));
  (* visit and modify by side effect env.g *)
  Graph_code_AST_visitor.map_program env ast |> ignore

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let verbose = true

(* TODO: too expensive to have all ASTs in memory? use lazy?
 * but then how to free memory between the 2 passes?
 * TODO: take also directory of stdlib and lazily index the defs in it
 *)
let build ~root ~hooks lang xs =
  let g = G.create () in
  G.create_initial_hierarchy g;
  let types = Hashtbl.create 101 in

  (* alt: let stats = G.empty_statistics () in *)
  let stats = Stat.empty_stats () in

  (*  let lookup_fails = Common2.hash_with_default (fun () -> 0) in *)
  let env_for_file phase file ast =
    let readable = if root == file then "<called_on_single_file>" else Common.readable ~root file in
    logger#info "readable: %s" readable;
    let current_parent, current_qualifier =
      Lang_specific.top_parent_and_qualifier ~lang ~readable ~ast
    in
    {
      g;
      stats;
      phase;
      hooks;
      current_parent;
      current_qualifier;
      file_or_package_qualifier = current_qualifier;
      class_qualifier = None;
      readable;
      lang;
      types;
    }
  in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  logger#info "STEP1: the definitions";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let env = env_for_file Defs file ast in
             extract_defs_uses env ast));

  logger#info "STEP2: the uses";
  xs
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun (file, ast) ->
             k ();
             let env = env_for_file Uses file ast in
             extract_defs_uses env ast));

  (g, stats)
