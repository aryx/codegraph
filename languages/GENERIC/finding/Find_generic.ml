(* Yoann Padioleau
 *
 * Copyright (C) 2022, 2025 Semgrep Inc.
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
 * history: similar to Find_target.ml in semgrep, and the old find_source.ml
 * in pfff.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let files_of_root ~(filter_file: (Fpath.t -> bool)) (lang : Lang.t) (root : Fpath.t) : Fpath.t list =
  let caps = Cap.readdir_UNSAFE () in
  List_files.list caps root
   |> List.filter filter_file
   |> List.filter (fun (file : Fpath.t) ->
      let langs = Lang.langs_of_filename file in
      List.mem lang langs
  )
