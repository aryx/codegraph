
(*
 *
 * history:
 *  - this was in pfff/lang_GENERIC/parse_generic.ml
 *  - this was moved in semgrep and generalized to Parse_target.ml to also
 *    use tree-sitter parsers
 *  - this was move back here and restricted back to pfff parsers to be
 *    used in codegraph/codecheck without depending on semgrep-core
 *)


(* See also Parse_target.parse_program in semgrep-core *)
let parse_program _lang _file =
  failwith "TODO"

(* coupling: mostly a copy paste of
 * Parse_target.parse_and_resolve_name_use_pfff_or_treesitter in semgrep-core *)
let parse_and_resolve_name _lang _file =
  failwith "TODO"
