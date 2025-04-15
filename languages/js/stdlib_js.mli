(* stdlib.js *)
val path_stdlib : Common2_.filename

(* generate stdlib.js *)
val extract_from_sources : Common2_.filename list -> Common2_.filename (* a dir *) -> unit
