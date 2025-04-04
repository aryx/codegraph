val resolve_path :
  root:Common2_.filename (* where to find node_modules *) ->
  pwd:Common2_.filename (* pwd of importer *) ->
  string ->
  (* full path *)
  Common2_.filename option
