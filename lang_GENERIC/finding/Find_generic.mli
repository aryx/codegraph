
val files_of_root: 
  filter_file:(Fpath.t -> bool) ->
  Lang.t -> Fpath.t (* the root *) -> Fpath.t list
