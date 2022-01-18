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
  | _ -> failwith (spf "lang %s not supported yet" (Lang.to_string lang))

(* coupling: mostly a copy paste of
 * Parse_target.parse_and_resolve_name_use_pfff_or_treesitter in semgrep-core *)
let parse_and_resolve_name _lang _file =
  failwith "TODO"
