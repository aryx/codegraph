
(* alt: we could compute the list of targets in build() itself, but
 * better to separate concerns I think. Targeting is actually tricky.
 *)
val build:
  root: Common.dirname ->
  Lang.t ->
  (Common.filename * AST_generic.program) list ->
  Graph_code.t * Graph_code.statistics

