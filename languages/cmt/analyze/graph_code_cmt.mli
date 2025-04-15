
val build:
  root:Common.filename ->
  cmt_files:Common.filename list -> 
  ml_files:Common.filename list ->
  Graph_code.t

(* for syncweb's indexer *)
val hook_def_node:
  (Graph_code.node -> Graph_code.t -> unit) ref

val hook_use_edge: 
  ((Graph_code.node * Graph_code.node) -> 
  Graph_code.t -> Tok.location -> unit) ref
