(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c, All rights reserved.
 *
 *)
open Graph_code_AST_env
module G = Graph_code
module E = Entity_code
module H = Graph_code_AST_helpers
module T = Resolved_type
module N = Resolved_name
module AST = AST_generic
module Stat = Stat_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lookup part of Graph_code_AST.ml
 *
 * Most of the functions in this file returns an option and so should
 * be used with a let* or let/ when called. Note that I do not
 * suffix those functions with _opt because there is always just
 * one function (not like in the stdlib where we have Hashtb.find and
 * Hashtbl.find_opt and so we need a way to say we want the exn or
 * option variant).
 *
 * alt:
 *  - the complex (but very general) stack lookup in stackgraph of github
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let lookup_resolved_name (env : env) (xs : N.t) : G.node option
    =
  Logs.info (fun m -> m "looking up: %s" (xs |> List.map fst |> String.concat "."));
  let g = env.g in
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | (str, _tk) :: xs -> (
        let children = G.children current g in
        let candidates =
          children
          |> List.filter (fun (s2, kind) ->
                 let xs =
                   match kind with
                   | E.Dir -> N.dotted_ident_of_dir s2
                   | _ -> N.dotted_ident_of_entname s2
                 in
                 N.last_ident xs = str)
        in
        match candidates with
        | [] ->
            let cur_str = G.string_of_node current in
            let lookup_fail = fst current ^ "." ^ str in
            Stat.add_failure env.stats ("lookup_resolved:" ^  lookup_fail);
            Logs.err (fun m -> m "no candidate found for %s at node %s, children = [%s]"
              str cur_str
              (children |> List.map G.string_of_node |> String.concat ","));
            None
        | [ y ] -> aux y xs
        | ys ->
            Logs.err (fun m -> m "too many candidates for %s, list = [%s]" str
              (ys |> List.map G.string_of_node |> String.concat ","));
            None)
  in
  aux G.root xs |> H.wrap_stat env "lookup_resolved"
  [@@profile]

let lookup_name_and_set_resolved_if_needed (env: env) (name: AST.name) : G.node option =
  let nopt = N.of_ast_name name in
  match nopt with
  | Some rn -> lookup_resolved_name env rn
  | None ->
     (match name with
     (* Maybe it's a local entity not resolved (yet) by Naming_AST.ml.
      * TODO: In some language (e.g., Python, Haskell) you can use
      * definitions defined later in the file, which is not handled by
      * Naming_AST. In other languages (e.g., OCaml, Rust?),
      * you can't and you can even define multiple times the same name.
      * Finally in other languages (e.g., C/C++) it depends whether it 
      * was "declared" via a prototype before. 
      * So what we do here is incorrect, but mostly correct in practice.
      *)
     | Id (id, id_info) ->
        (match !(id_info.id_resolved) with
        | None
        | Some (Global, _) ->
            let rn = env.file_or_package_qualifier @ [ id ] in
            (match lookup_resolved_name env rn with
            (* good it exists! *)
            | Some n ->
                N.set_resolved_if_none name (List.map fst rn);
                Some n
            | None -> None
            (* TODO? ImportedModule Filename => lookup E.File *)
            )
        | _ -> None
        )
     | IdQualified _ -> None
     (* TODO? *)
     | IdSpecial _ -> None
     )
  

let lookup_type_of_node env node =
  Logs.info (fun m -> m "lookup type for node %s" (G.string_of_node node));
  Hashtbl.find_opt env.types node |> H.wrap_stat env "lookup_type"

let lookup_resolved_type_of_resolved_name env rn =
  (Logs.info (fun m -> m "looking up type for: %s" (N.to_entname rn));
  let* n = lookup_resolved_name env rn in
  let* t = lookup_type_of_node env n in
  Logs.info (fun m -> m "found type for %s = %s" (G.string_of_node n) (T.show t));
  Some t) |> H.wrap_stat env "lookup_resolved_type_of_resolved_name"
