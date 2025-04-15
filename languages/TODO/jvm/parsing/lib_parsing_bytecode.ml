(* Copyright (C) 2012 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

module FT = File_type

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  |> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | FT.Obj ("class") -> true
    | _ -> false
  ) |> Common.sort
