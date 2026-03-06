val gen_rank_heatmap_layer :
  Graph_code.t ->
  (Graph_code.node, int) Hashtbl.t ->
  output:Common2.filename ->
  unit

val gen_statistics_layer :
  root:Fpath.t -> Graph_code.statistics -> output:Common2.filename -> unit

val actions : unit -> Arg_.cmdline_actions
