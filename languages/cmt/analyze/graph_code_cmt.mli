
val build:
  Fpath.t (* root *) ->
  cmt_files:Common2_.filename list -> 
  ml_files:Common2_.filename list ->
  Graph_code.t

(* for syncweb's indexer *)
val hook_def_node:
  (Graph_code.node -> Graph_code.t -> unit) ref

val hook_use_edge: 
  ((Graph_code.node * Graph_code.node) -> 
  Graph_code.t -> Tok.location -> unit) ref
