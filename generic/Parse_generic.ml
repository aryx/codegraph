(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *
 * history:
 *  - this was in pfff/lang_GENERIC/parse_generic.ml
 *  - this was moved in semgrep and generalized to Parse_target.ml to also
 *    use tree-sitter parsers
 *  - this was move back here and restricted back to pfff parsers to be
 *    used in codegraph/codecheck without depending on semgrep-core
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* See also Parse_target.parse_program in semgrep-core *)
let parse_program lang file =
  match lang with
  | Lang.Python -> 
      let ast = Parse_python.parse_program file in
      Python_to_generic.program ast
  | Lang.Java -> 
      let ast = Parse_java.parse_program file in
      Java_to_generic.program ast
  | Lang.Ts -> 
      let ast = Parse_js.parse_program file in
      Js_to_generic.program ast
  | Lang.Ocaml -> 
      let ast = Parse_ml.parse_program file in
      Ml_to_generic.program ast
  | _ -> failwith (spf "lang %s not supported yet" (Lang.to_string lang))

(* coupling: mostly a copy paste of
 * Parse_target.parse_and_resolve_name_use_pfff_or_treesitter in semgrep-core *)
let parse_and_resolve_name lang file =
  let ast = parse_program lang file in
  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  AST_generic_helpers.gensym_counter := 0;
  Naming_AST.resolve lang ast;
  ast
