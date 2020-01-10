(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

module E = Entity_code
module GC = Graph_code
module GC2 = Graph_code_opti
module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* This program builds the graph database needed by codegraph (and also
 * leveraged now by codemap).
 * See main_codegraph.ml for more information.
 * 
 * todo?
 *  - merge with pfff_db.ml?
 * 
 * history:
 *  - split from main_codegraph.ml
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

let lang = ref "ml"

let output_dir = ref None
(* generate also tags, light db, layers, etc. *)
let gen_derived_data = ref false
(* not perfect ... *)
let class_analysis = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let dep_file_of_dir dir = 
  Filename.concat dir Graph_code.default_filename

let set_gc () =
  (* only relevant in bytecode, in native the stacklimit is the os stacklimit*)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 4_000_000 };
  (* goes from 5300s to 3000s for building db for www *)
  Gc.set { (Gc.get()) with Gc.major_heap_increment = 8_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 300 };
  ()

(*****************************************************************************)
(* Building stdlib *)
(*****************************************************************************)
let build_stdlib lang root dst =
  let files = Find_source.files_of_root ~lang root in
  match lang with
  | "java" ->
      Builtins_java.extract_from_sources ~src:root ~dst files
  | "js" ->
      Stdlib_js.extract_from_sources files dst
  | _ -> failwith ("language not supported: " ^ lang)


(*****************************************************************************)
(* Main action, building the graph *)
(*****************************************************************************)

let main_action xs =
  set_gc ();
  let lang = !lang in

  let xs = List.map Common.fullpath xs in
  let root, files = 
    match xs with
    | [root] -> 
        root, Find_source.files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = Find_source.files_of_dir_or_files ~lang xs in
        root, files
  in

  let empty = Graph_code.empty_statistics () in
  let g, stats =
    try (
    match lang with
    | "ml"  -> Graph_code_ml.build ~verbose:!verbose root files, empty
    | "lisp" -> Graph_code_lisp.build ~verbose:!verbose root files, empty

    | "c" -> 
        Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
        let local = Filename.concat root "pfff_macros.h" in
        if Sys.file_exists local
        then Parse_cpp.add_defs local;
        Graph_code_c.build ~verbose:!verbose root files, empty

    | "java" -> Graph_code_java.build ~verbose:!verbose root files, empty

    | "php" -> 
      (* todo: better factorize *)
      let skip_file = Filename.concat root "skip_list.txt" in
      let skip_list =
        if Sys.file_exists skip_file
        then Skip_code.load skip_file
        else []
      in
      let is_skip_error_file = Skip_code.build_filter_errors_file skip_list in
      Graph_code_php.build 
        ~verbose:!verbose ~is_skip_error_file 
        ~class_analysis:!class_analysis
        root files
    | "js" -> Graph_code_js.build ~verbose:!verbose root files, empty

    | "dot" -> 
      Graph_code.graph_of_dotfile (Filename.concat root "graph.dot"), empty

    | _ -> failwith ("language not supported: " ^ lang)
    )
    with (Graph_code.Error err) as exn ->
      pr2 (Graph_code.string_of_error err);
      raise exn
  in
  let output_dir = !output_dir ||| root in
  let file = dep_file_of_dir output_dir in
  Graph_code.save g file;
  Graph_code.print_statistics stats g;
  Common.pr2 (spf "Saving codegraph file in %s" file);

  (* Save also TAGS, light db, prolog (TODO), layers. We could also do
   * that on demand when we run codemap and there is only a 
   * graph_code.marshall file.
   * TODO: save in a .pfff/
   *)
  if !gen_derived_data then begin
    let p f = Filename.concat output_dir f in
    Layer_graph_code.gen_rank_heatmap_layer g (GC.bottom_up_numbering g) 
      (p "layer_bottomup.json");
    Layer_graph_code.gen_rank_heatmap_layer g (GC.top_down_numbering g) 
      (p "layer_topdown.json");
    Layer_graph_code.gen_statistics_layer ~root stats 
      ~output:(p "layer_graphcode_stats.json");
    let defs = Graph_code_tags.defs_of_graph_code g in
    Tags_file.generate_TAGS_file (p "TAGS") defs;
    let db = Graph_code_database.db_of_graph_code root g in
    Database_code.save_database db (p "PFFF_DB.marshall");
    Database_code.save_database db (p "PFFF_DB.json");
  end;
  ()

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(* Analysis *)
let analyze_backward_deps graph_file =
  let g = GC.load graph_file in
  let gopti = 
    Common.cache_computation ~verbose:!verbose graph_file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  let config = DM.basic_config_opti gopti in
  (* DM.threshold_pack := 90; *)
  let config = DM.expand_node_opti (("flib", E.Dir)) config gopti in
  let dm, gopti = DMBuild.build config None gopti in
  let n = Array.length dm.DM.matrix in
  (* todo: filter biggest offenders? *)
  let res = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let n = dm.DM.matrix.(i).(j) in
      if n > 0 then begin
        let xs = DM.explain_cell_list_use_edges (i, j) dm gopti in
        pr2 (spf " (%d, %d) = %d" i j (List.length xs));
        Common.push xs res;
      end
    done
  done;
  let edges = List.flatten !res in
  pr2 (spf "total backward deps = %d" (List.length edges));
  let xxs = Common.group_by_mapped_key (fun (_n1, n2) -> n2) edges in
  pr2 (spf "#dst =%d" (List.length xxs));
  xxs |> List.map (fun (n, xs) -> (n, xs), List.length xs)
    |> Common.sort_by_val_highfirst
    |> Common.take_safe 100
    |> List.iter (fun ((n, _xs), cnt) ->
        let file = GC.file_of_node n g in
         pr2 (spf "%-30s = %d (file = %s)" (GC.string_of_node n) cnt
                file)
    );
  let file = graph_file ^ ".whitelist" in
  pr2 (spf "generating whitelist in %s" file);
  GC.save_whitelist edges file g;
    ()


(* Graph adjuster (overlay-ish) *)
let adjust_graph graph_file adjust_file whitelist_file dest_file =
  let g = Graph_code.load graph_file in
  let adjust = Graph_code.load_adjust adjust_file in
  let whitelist = Graph_code.load_whitelist whitelist_file in
  Graph_code.adjust_graph g adjust whitelist;
  Graph_code.save g dest_file;
  ()

(* quite similar to analyze_backward_deps *)
let test_thrift_alive graph_file =
  let g = GC.load graph_file in
  let gopti = 
    Common.cache_computation ~verbose:!verbose graph_file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  let config = DM.basic_config_opti gopti in
  DMBuild.threshold_pack := max_int;
  let config = DM.expand_node_opti (("lib", E.Dir)) config gopti in
  let config = DM.expand_node_opti (("lib/thrift", E.Dir)) config gopti in
  let config = DM.expand_node_opti (("lib/thrift/packages", E.Dir)) config gopti in
  let dm, gopti = DMBuild.build config None gopti in
  let n = Array.length dm.DM.matrix in

  let kflib = Hashtbl.find dm.DM.name_to_i ("flib", E.Dir) in
  for j = 0 to n - 1 do
    let (s, _kind) = dm.DM.i_to_name.(j) in
    if s =~ "lib/thrift/packages/.*"
    then begin
      let v = dm.DM.matrix.(kflib).(j) in
      if v > 0 then begin
        pr2 (spf "%s is USED in flib/" s);
        let xs = DM.explain_cell_list_use_edges (kflib, j) dm gopti in
        xs |> Common.take_safe 5 |> List.iter (fun (n1, n2) ->
          pr2 (spf "    %s --> %s" 
                 (GC.string_of_node n1) (GC.string_of_node n2))
        )
      end else begin
        if DM.is_dead_column j dm
        then begin
          pr2 (spf "%s appeared DEAD" s);
        end
      end
    end
  done

(* quite similar to analyze_backward_deps *)
let test_adhoc_deps graph_file =
  let g = GC.load graph_file in
  g |> GC.iter_use_edges (fun n1 n2 ->
    let file = GC.file_of_node n2 g in
    if file =~ ".*flib/intern/thrift/lib"
    then begin
      let file2 = GC.file_of_node n1 g in
      if file2 =~ ".*tests/" || file2 =~ ".*/__tests__/.*"
      then begin
        pr2_once (spf "%s --> %s" file2 file);
        (*pr2 (spf " %s --> %s" (GC.string_of_node n1) (GC.string_of_node n2));*)
      end
    end
  )

let test_layering graph_file =
  let g = GC.load graph_file in
  let (scc, _hscc) = GC.strongly_connected_components_use_graph g in
  pr2 (spf "#scc = %d" (Array.length scc));
  let htopdown = GC.bottom_up_numbering g in
  pr2 (spf "computed numbering = %d" (Hashtbl.length htopdown));
  let xs = htopdown |> Common.hash_to_list |> List.map snd in
  let min = Common2.minimum xs in
  assert(min = 0);
  let max = Common2.maximum xs in
  pr2 (spf "max = %d" max);
  
  let file = "/tmp/rank_code.txt" in
  pr2 (spf "ranks in %s" file);
  Common.with_open_outfile file (fun (pr, _chan) ->
    let pr s = pr (s ^ "\n") in
    htopdown |> Common.hash_to_list |> Common.sort_by_val_lowfirst
    |> List.iter (fun (node, v) -> 
      pr (spf "%s: %d" (GC.string_of_node node) v)
    )
  );

  let (d,_,_) = Common2.dbe_of_filename graph_file in
  let output = Common2.filename_of_dbe (d, "layer_graph_code", "json") in
  Layer_graph_code.gen_rank_heatmap_layer g htopdown output;
  ()


let test_xta graph_file = 
  let g = Graph_code.load graph_file in
  let dag = Graph_code_class_analysis.class_hierarchy g in
  let hdepth = Graphe.depth_nodes dag in
  hdepth |> Hashtbl.iter (fun k v ->
    pr2 (spf "%s = %d" (Graph_code.string_of_node k) v);
  );
  let dag = Graph_code_class_analysis.class_hierarchy g in
  let htoplevels = Graph_code_class_analysis.toplevel_methods g dag in
  htoplevels |> Common2.hkeys |> List.iter (fun k ->
      let xs = Hashtbl.find_all htoplevels k in
      pr2 (spf "%s -> %d (e.g. %s)" 
               k (List.length xs) (Graph_code.string_of_node (List.hd xs)));
  );
  ()

let test_dotfile_of_deps dir =
  let deps = Common.cmd_to_list (spf "find %s -name \"*.deps\" " dir) in
  deps |> List.iter (fun file ->
    let (_d,lib,e) = Common2.dbe_of_filename file in
    if e = "deps" then begin
      let deps = Common.cat file in
      deps |> List.iter (fun lib2 ->
        pr (spf "\"%s\" -> \"%s\"" lib lib2)
      )
    end
  )  

(* This is actually not great because the .depend contains the dependencies
 * for the .o and so it does not contain inter .h dependencies.
 * Use http://www.flourish.org/cinclude2dot/ instead.
 *)
let test_dotfile_of_dotdepend file =
  let rec remove_antislash xs =
    match xs with
    | [] -> []
    | x::y::xs ->
      if x =~ "\\(.*\\)\\\\$"
      then remove_antislash ((Common.matched1 x ^ y)::xs)
      else x::(remove_antislash (y::xs))
    | [x] -> [x]
  in
  let lines = Common.cat file in
  let lines = remove_antislash lines in
  (* lines |> List.iter pr2_gen *)
  lines |> List.iter (fun line ->
    match () with
    | _ when line =~ ".*\\.o: \\([^ ]+\\)\\.cpp\\(.*\\)$" ->
      let (src, deps_str) = Common.matched2 line in
      let deps = Common.split "[ \t]+" deps_str in
      deps |> List.iter (fun dep ->
        pr (spf "\"%s.cpp\" -> \"%s\""  src dep)
      )
    | _ when line =~ ".*\\.o: \\([^ ]+\\)\\.h\\(.*\\)$" ->
      let (src, deps_str) = Common.matched2 line in
      let deps = Common.split "[ \t]+" deps_str in
      deps |> List.iter (fun dep ->
        pr (spf "\"%s.cpp\" -> \"%s\""  src dep)
      )

    | _ -> failwith (spf "wront line format in .depend: %s" line)
  )


(* ---------------------------------------------------------------------- *)
let extra_actions () = [

  "-to_json", " <graph file>", 
  Common.mk_action_1_arg (fun file ->
    let g = Graph_code.load file in
    let json = Graph_code_export.graph_to_json g in
    let dst = "graph_code.json" in
    pr2 (spf "saving graph in JSON format in %s" dst);
    Json_io.string_of_json ~compact:false ~recursive:false ~allow_nan:true json
     |> Common.write_file ~file:dst
  );

  "-build_stdlib", " <src> <dst>",
  Common.mk_action_2_arg (fun dir dst -> build_stdlib !lang dir dst);
  "-adjust_graph", " <graph> <adjust_file> <whitelist> <dstfile>\n",
  Common.mk_action_4_arg (fun graph file file2 dst -> 
    adjust_graph graph file file2 dst);


  "-test_backward_deps", " <graph>",
  Common.mk_action_1_arg (fun graph_file -> 
    analyze_backward_deps graph_file
  );
  "-test_protected_to_private", " <graph>",
  Common.mk_action_1_arg (fun graph_file ->
    let g = Graph_code.load graph_file in
    Graph_code_class_analysis.protected_to_private_candidates g
  );
  "-test_thrift_alive", " <graph>",
  Common.mk_action_1_arg test_thrift_alive;
  "-test_pad", " <graph>",
  Common.mk_action_1_arg test_adhoc_deps;
  "-test_layering", " <graph>",
  Common.mk_action_1_arg test_layering;
  "-test_xta", " <graph>",
  Common.mk_action_1_arg test_xta;
  "-test_dotfile_of_deps", " <dir>",
  Common.mk_action_1_arg test_dotfile_of_deps;
  "-test_dotfile_of_dotdepend", " <file>",
  Common.mk_action_1_arg test_dotfile_of_dotdepend;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions () @
  []

let options () = [
  "-lang", Arg.Set_string lang, 
  (spf " <str> choose language (default = %s) " !lang);
  "-o", Arg.String (fun s -> output_dir := Some s), 
  " <dir> save graph_code.marshall in another dir";
  "-derived_data", Arg.Set gen_derived_data, 
  " generate also TAGS, layers, light db, etc.";

  "-class_analysis", Arg.Set class_analysis, 
  " resolve some method calls";

  "-symlinks", Arg.Unit (fun () -> Common.follow_symlinks := true;), 
  " follow symlinks";
 
  "-no_fake_node", Arg.Clear Graph_code_php.add_fake_node_when_undefined_entity,
  " no fake nodes when use-def mismatches\n";
  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
  "-verbose", Arg.Unit (fun () ->
    verbose := true;
    DM.verbose := true;
  ), " ";

  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "CodeGraph version: %s" Config_pfff.version);
    exit 0;
  ), 
  " guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codegraph"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 
    (match args with
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
