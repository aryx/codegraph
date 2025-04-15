(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 *)
open Common
module G = Graph_code
module E = Entity_code
module H = Graph_code_AST_helpers
module N = Resolved_name
open AST_generic

(* open Graph_code_AST_env *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Every Lang-specific code should be in this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Lang-specific helpers *)
(*****************************************************************************)

let top_parent_and_qualifier ~lang ~readable ~ast :
    G.node * AST_generic.dotted_ident =
  match lang with
  (* in Python, the directory and filename implicitely defines a package *)
  | Lang.Python ->
      let tk = Tok.first_tok_of_file (Fpath.v readable) in
      (* ex: from a/b/foo.py, we want to return
       *   - the parent node (a/b/foo, E.File) (without .py extension)
       *   - the qualifiers [a;b;foo]
       *)
      let d, b, e = Filename_.dbe_of_filename readable in
      (* coupling: ugly hack for pycheck; we should move out in a hook *)
      let b =
        match (b, e) with
        | s, "pyi" when s =~ "^\\(.*\\)_$" -> Common.matched1 s
        | _ -> b
      in
      let dotted_idents =
        if d = "." then
          [ (b, tk) ]
        else
          String_.split ~sep:"/" d @ [ b ] |> List.map (fun s -> (s, tk))
      in
      (* basically replacing "/" with "." *)
      let entname = N.to_entname dotted_idents in
      let node = (entname, E.File) in
      (node, dotted_idents)
  (* in Java, packages are explicit construct *)
  | Lang.Java -> (
      match ast with
      | {
          s = DirectiveStmt { d = Package (_tk, dotted_idents); d_attrs = _ };
          _;
        }
        :: _ ->
          let entname = N.to_entname dotted_idents in
          let node = (entname, E.Package) in
          (node, dotted_idents)
      (* can this happen in practice? for test files maybe? *)
      (* for scripts, tests, or entry points? *)
      | _ ->
          let node = (readable, E.File) in
          (node, []))
  | Lang.Ruby ->
    (* temporary hack where we assume a flat namespace and don't process
     * the require "foo", require_relative "bar", etc.
     *)
    G.root, []

  (* TODO: in OCaml, it's a flat namespace by default; the path of
   * the module does not matter (when '(wrapped false)' is defined
   * in the dune file), but the filename defines the module name
   * (with some capitalization done automatically if needed)
   *)
  | l -> failwith (spf "language not handled yet: %s" (Lang.to_string l))
