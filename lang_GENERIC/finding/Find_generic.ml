(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* File targeting for languages supported by AST_generic.ml
 *
 * Similar to Find_target.ml in semgrep-core, and 
 * the old find_source.ml in pfff.
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let files_of_root lang root =
  let files = 
    Common.files_of_dir_or_files_no_vcs_nofilter [root] |> File.Path.of_strings in
  let files, _skipped = 
    Skip_code.filter_files_if_skip_list ~root:[Fpath.v root] files in
  files |> File.Path.to_strings |> List.filter (fun file ->
      let langs = Lang.langs_of_filename (Fpath.v file) in
      List.mem lang langs
  )
