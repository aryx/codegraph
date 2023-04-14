(* UPDATE: this is mostly obsolete. You should use Find_target.ml instead. *)

let logger = Logging.get_logger [__MODULE__]

let finder lang =
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

let skip_file dir =
  Filename.concat dir "skip_list.txt"


let files_of_dir_or_files ~lang xs =
  let finder = finder lang in
  let xs = List.map Common.fullpath xs |> File.Path.of_strings in
  finder xs |> Skip_code.filter_files_if_skip_list ~root:xs |> fst
  |> File.Path.to_strings


(* todo: factorize with filter_files_if_skip_list?
 * less: a ~verbose argument to not always display the pr2 below?
*)
let files_of_root ~lang root =
  let finder = finder lang in
  let files = finder [Fpath.v root] in

  let skip_list =
    if Sys.file_exists (skip_file root)
    then begin
      logger#info "Using skip file: %s (for lang = %s)" (skip_file root) lang;
      Skip_code.load (Fpath.v (skip_file root));
    end
    else []
  in
  Skip_code.filter_files skip_list ~root:(Fpath.v root) files |> fst
  |> File.Path.to_strings

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
