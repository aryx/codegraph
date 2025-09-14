(* Yoann Padioleau
 *
 * Copyright (C) 2025 Yoann Padioleau
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
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Yet another file targeting helper.
 *
 * See also Find_generic.ml and Find_targets.ml in semgrep 
*)

(*****************************************************************************)
(* Legacy helpers (were in languages/<lang>/ast/Lib_parsing_<lang>.ml)  *)
(*****************************************************************************)

let find_cmt_files_of_dir_or_files (xs : Fpath.t list) : Fpath.t list =
  let caps = Cap.readdir_UNSAFE () in
  UFile.files_of_dirs_or_files_no_vcs_nofilter caps xs
  |> List.filter (fun filename ->
         match Ftype.of_file filename with
         | Ftype.Obj ("cmt" | "cmti") -> true
         | _ -> false)
  (* ocaml 4.07 stdlib now has those .p.cmt files that cause dupe errors *)
  |> List_.exclude (fun filename -> !!filename =~ ".*\\.p\\.cmt")
  (* sometimes there is just a .cmti and no corresponding .cmt because
   * people put the information only in a .mli
   *)
  |> (fun xs ->
       let hfiles = Hashtbl.create 101 in
       xs
       |> List.iter (fun file ->
              let d, b, e = Filename_.dbe_of_filename !!file in
              Hashtbl_.push hfiles (d, b) e);
       Common2_.hkeys hfiles
       |> List_.map (fun (d, b) ->
              let xs = Hashtbl_.get_stack hfiles (d, b) in
              match xs with
              | [ "cmt"; "cmti" ]
              | [ "cmti"; "cmt" ]
              | [ "cmt" ] ->
                  Filename_.filename_of_dbe (d, b, "cmt")
              | [ "cmti" ] -> Filename_.filename_of_dbe (d, b, "cmti")
              | _ -> raise Impossible))
  |> Fpath_.of_strings |> List_.sort

let find_ml_files_of_dir_or_files xs =
  let caps = Cap.readdir_UNSAFE () in
  UFile.files_of_dirs_or_files_no_vcs_nofilter caps xs
  |> List.filter (fun filename ->
         match Ftype.of_file filename with
         | Ftype.PL (Ftype.OCaml "ml") -> true
         | _ -> false)
  |> List_.sort

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let files_of_root ~lang (root : Fpath.t) : string list =
  match lang with
  | "cmt" -> 
      find_cmt_files_of_dir_or_files [root] |> Fpath_.to_strings
  | "ml" ->
      find_ml_files_of_dir_or_files [root] |> Fpath_.to_strings
      
  | _ -> failwith (spf "Find_source.files_of_root: language not handled yet %s" lang)


(*****************************************************************************)
(* OLD STUFF *)
(*****************************************************************************)


(* UPDATE: this is mostly obsolete. You should use Find_target.ml instead. *)

let finder _lang _xs =
  failwith "TODO: finder"
(*
  match lang with

  (* in pfff *)
  | "php" | "phpfuzzy" | "php2" ->
      Lib_parsing_php.find_source_files_of_dir_or_files ~verbose:false ~include_hack:false
  | "hack" ->
      Lib_parsing_php.find_source_files_of_dir_or_files ~verbose:false ~include_hack:true
  | "c++" ->
      Lib_parsing_cpp.find_source_files_of_dir_or_files
  | "c" ->
      Lib_parsing_c.find_source_files_of_dir_or_files
  | "ml" | "ocaml" | "mlfuzzy" ->
      Lib_parsing_ml.find_source_files_of_dir_or_files
  | "java" | "javafuzzy" ->
      Lib_parsing_java.find_source_files_of_dir_or_files
  | "js" | "javascript" | "jsfuzzy" | "jsgen"  ->
      Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false
  | "py" | "python"  ->
      Lib_parsing_python.find_source_files_of_dir_or_files
  | "lisp" ->
      Lib_parsing_lisp.find_source_files_of_dir_or_files
  | "dot" -> (fun _ -> [])

  (* in pfff-bytecode *)
  | "cmt"  -> 
    Lib_parsing_ml.find_cmt_files_of_dir_or_files

  | _ -> failwith ("Find_source: unsupported language: " ^ lang)
*)


let files_of_dir_or_files ~lang xs =
  ignore(lang, xs);
  failwith "TODO: files_of_dir_or_files"
  (* TODO: use Gitignore like in codemap *)
(*
  let finder = finder lang in
  let xs = List.map Common.fullpath xs |> Fpath_.of_strings in
  finder xs |> Skip_code.filter_files_if_skip_list ~root:xs |> fst
  |> File.Path.to_strings
*)












(* todo: factorize with filter_files_if_skip_list?
 * less: a ~verbose argument to not always display the pr2 below?
*)
(*
let files_of_root ~lang (root : Fpath.t) =
  let finder = finder lang in
  let _files = finder [root] in
  failwith "TODO: files_of_root"
*)
(*

let skip_file dir =
  Filename.concat dir "skip_list.txt"
...
  let skip_list =
    if Sys.file_exists (skip_file root)
    then begin
      Logs.info (fun m -> m "Using skip file: %s (for lang = %s)" (skip_file root) lang);
      Skip_code.load (Fpath.v (skip_file root));
    end
    else []
  in
  Skip_code.filter_files skip_list ~root:(Fpath.v root) files |> fst
  |> File.Path.to_strings
*)

(*
  let root = Common.realpath dir in
  let all_files = Lib_parsing_clang.find_source2_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  (* step0: reorder files *)
  let files = Skip_code.reorder_files_skip_errors_last skip_list root files in

  let root = Common.realpath dir_or_file in
  let all_files =
    Lib_parsing_bytecode.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files =
    Skip_code.filter_files skip_list root all_files in

  let root = Common.realpath dir in
  let all_files = Lib_parsing_c.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in


  let root = Common.realpath dir_or_file in
  let all_files = Lib_parsing_java.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in

  let root = Common.realpath dir in
  let all_files = Lib_parsing_ml.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in


  let root = Common.realpath dir in
  let all_files = Lib_parsing_cpp.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in

  let root, files =
    Common.profile_code "Graph_php.step0" (fun () ->
    match dir_or_files with
    | Left dir ->
        let root = Common.realpath dir in
        let files =
          Lib_parsing_php.find_php_files_of_dir_or_files [root]
          +> Skip_code.filter_files skip_list root
          +> Skip_code.reorder_files_skip_errors_last skip_list root
        in
        root, files
    (* useful when build codegraph from test code *)
    | Right files ->
        "/", files
    )
  in




  let root, files =
    match dir_or_files with
    | Left dir ->
      let root = Common.realpath dir in
      let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in

      (* step0: filter noisy modules/files *)
      let files =
        Skip_code.filter_files skip_list root all_files in
      (* step0: reorder files *)
      let files =
        Skip_code.reorder_files_skip_errors_last skip_list root files in
      root, files
    (* useful when build from test code *)
    | Right files ->
      "/", files
  in

  let skip_file = !skip_list ||| skip_file_of_dir root in
  let skip_list =
    if Sys.file_exists skip_file
    then begin
      pr2 (spf "Using skip file: %s" skip_file);
      Skip_code.load skip_file
    end
    else []
  in
  let finder = Find_source.finder lang in

    let skip_file = "skip_list.txt" in
    let skip_list =
      if Sys.file_exists skip_file
      then begin
        pr2 (spf "Using skip file: %s" skip_file);
        Skip_code.load skip_file
      end
      else []
    in
*)
