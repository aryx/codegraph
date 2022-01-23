
(* This only uses pfff-parsers. To use tree-sitter parsers you'll need
 * to use Parse_target.ml in semgrep, but this means your program now
 * depends also on semgrep.
 *)
val parse_program: Lang.t -> Common.filename -> AST_generic.program

(* This calls Naming_AST.ml in semgrep-core/src/analyzing/ to annotate
 * the AST with naming information. It essentially resolves local names.
 * To fully resolve, see Graph_code_generic.ml
 *)
val parse_and_resolve_name: Lang.t -> Common.filename -> AST_generic.program
