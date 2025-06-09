(* UPDATE: this is mostly obsolete. You should use Lang.files_of_dirs_or_files
 * instead.
*)

(* will manage optional skip list at root *)
val files_of_root:
  lang:string ->
  Fpath.t (* root dir *) -> Common2_.filename list

(* will manage optional skip list at root of vcs *)
val files_of_dir_or_files:
  lang:string ->
  Common2_.filename list -> Common2_.filename list


val finder: string -> (Fpath.t list -> Fpath.t list)
