val build :
  ?verbose:bool ->
  (* for builtins_java.ml, tags_java.ml *)
  ?only_defs:bool ->
  Fpath.t (* root dir *) ->
  Common2_.filename list ->
  Graph_code.t
