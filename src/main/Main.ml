(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common
open Fpath_.Operators
module GC = Graph_code
module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* This is the main entry point of Codegraph, a package/module/type/function/...
 * hierarchical dependency visualizer using mainly a Dependency
 * Structure Matrix (DSM). 
 *
 * This should help understand the "software architecture" of a project
 * and assist in refactoring it.
 * A node-link display of hierarchical graphs (or hypergraphs) would be nice
 * too, but it is far more complex to draw than matrices and does
 * not scale as well visually apparently.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * A great introduction on the use of DSM for improving a codebase is
 * http://codebetter.com/patricksmacchia/2009/08/24/identify-code-structure-patterns-at-a-glance/
 * 
 * IMHO spending time to have clean dependencies is one of the most
 * important thing to do in a software project.
 * "Organizing complexity is the most important skill"
 * http://www.johndcook.com/blog/2015/06/18/most-important-skill-in-software/
 * 
 * It seems there are a few commercial projects using DSM (Ndepend,
 * Structure101, Intellij), so this looks like a viable direction to pursue
 * to visualize a software architecture.
 * 
 * requirements:
 *  - different granularities for x-to-x relationships
 *    (packages to packages, modules to packages, functions, constructors, etc),
 *    so one can get:
 *    * package (or directory) projection to reduce the size of the graph and
 *      get a high-level view of the architecture ("package mode")
 *    * with or without external/ dependencies
 *    * possibiltiy to get a slice of the graph for just a directory
 *      with a package (or directory) projection for external dependencies
 *      ("module mode")
 *    * possiblity to zoom to see the actual functions of a package involved
 *      in a dependency. This is especially useful for edges where
 *      we don't understand why there exists a dependency.
 *  - variable arrow size (but the count number in the matrix does that too)
 *  - variable node size (but the count number in same row does that too)
 * 
 * This tool also contains some actions to generate data for different
 * graph visualizer, e.g. Gephi, Guess. todo? backend for Graphviz? Phylomel?
 * old: $ pm_depend [-lang X] [-with-extern] [-depth n] -o filename /path/dir
 * 
 * related work: 
 * - Lattix, the startup of the original paper on DSM at OOPSLA'05
 * 
 * - Ndepend, http://www.ndepend.com/Doc_VS_Arch.aspx
 *   http://codebetter.com/patricksmacchia/2009/08/24/identify-code-structure-patterns-at-a-glance/
 * - Structure101, http://www.headwaysoftware.com/products/index.php#page-top
 * - Intellij IDEA dsm tool
 *   http://blogs.jetbrains.com/idea/2008/01/intellij-idea-dependency-analysis-with-dsm/
 *   http://www.jetbrains.com/idea/features/dependency_analysis.html
 * 
 * - http://depfind.sourceforge.net/, a dependency extraction tool for Java
 * - https://github.com/sourcegraph/graphkit, a generic format for code 
 *   dependencies and language specific "graphers" (ruby, js, go, python)
 * 
 * - http://mcis.polymtl.ca/~bram/makao/index.html also use GUESS and Prolog :)
 * - http://infotectonica.com/juliet/tour/, seems more oriented on
 *   query, anserwing questions like who uses this field.
 * 
 *  - visual studio dependency graph visualizer:
 *    http://msdn.microsoft.com/en-us/library/vstudio/dd409365.aspx
 *    http://channel9.msdn.com/Series/Visual-Studio-2012-Premium-and-Ultimate-Overview/Visual-Studio-Ultimate-2012-Understand-your-code-dependencies-through-visualization
 * 
 *  - a few visualizations applied on go dependencies (including chords)
 *    http://dave.cheney.net/2014/11/21/visualising-dependencies
 * 
 * - google search images: dependency+graph+visualization, get many
 *   links from there 
 * 
 * history:
 *  - quick look at work on software architecture because of Banatre
 *    while a master student at IRISA, and later Schmidt, while a PhD,
 *    looking at work of Shaw and Garlan and the different
 *    architecture patterns (whiteboard, pipe, layers, etc).
 *  - started to draw hypergraphs of architecture while supervising a
 *    student project at EMN (submarine game), and advocated they were
 *    better than UML diagrams (I think after reading Harel's papers on
 *    history of statecharts in HOPL-III)
 *  - dir to dir dependencies during coccinelle project? 
 *    Projections were hardcoded each time for each use. 
 *    No generic framework (like the hierarchical dependency matrix).
 *    Done for C (then for PHP later, and then for OCaml far later).
 *  - very nice picture of architecture of Linux kernel sent by Gilles,
 *    the "map of the Linux kernel"
 *  - found that having a graph of module dependencies was very useful
 *    when refactored the c-- and mmm codebase, thx to ocamldot.
 *    But felt the need to have variable-size arrows (and nodes) and also
 *    the ability to get more information when clicking on an edge, 
 *    to actually see what are the functions involved in a dependency
 *    for instance.
 *  - flibotonomy by Greg Scheschte for PHP, but focused on the nodes
 *    instead of the edges (which I think are more important).
 *  - overlay, and cmf -y to display dependencies at "package" level
 *  - pm_depend, ocaml dependencies backend, ~package_depth, ~with_extern.
 *    In some ways it extracts the dependency information I have 
 *    in my Makefiles where I care about the order of the directories
 *    and files. The ~package_depth and ~with_extern parameters are just
 *    special cases of the general idea of displaying at different
 *    granularity dependencies depending on the directory.
 *    Finally it was limited to just package/module (pm_depend) but quickly
 *    you want to know the actual functions/classes that are involved in
 *    a dependency.
 *  - gephi/guess visualization, but even with -no_extern, it does not
 *    scale very well for www. It's ok for pfff, but even for 
 *    the full source of pfff the graph is quite noisy.
 *  - discover DSM of ndepend.com while doing a google search images on
 *    "dependency+graph+visualization"
 * 
 *  - gradually realize the importance of layered structures
 *    (which are actually enforced in OCaml by the linker)
 *  - gradually realize the importance of dependencies and how
 *    they are at the essence of software architecture. Code is
 *    a tree when looked locally (AST), but it's really a graph
 *    of dependencies when looked globally.
 * 
 * todo: 
 *  - can codegraph does a good job to convey the software architecture
 *    of codegraph itself? does it show clearly that Graph_code.graph and
 *    dependencies_matrix_code.dm are the essential data structures? And
 *    can it show the important fields?
 *  - maybe edge-bundles could make the node-link display approach
 *    scale better.
 *  - generate a node-link graph for the current matrix configuration; use
 *    graphviz as a first step.
 *  - http://redotheweb.com/DependencyWheel/?
 *  - https://github.com/graphistry/pygraphistry big graph visualizer
 * 
 * 
 * (comments that were in graph_modules_packages_ml.ml)
 * alternatives:
 *  - ocamldoc -dot ...
 *    But if there is one parse error or a module not found, then 
 *    ocamldoc fails. Also there is no package "projection" so it's 
 *    hard to apply on large projects. There is also no with-extern view
 *    with the slice of the graph to a directory.
 *    TODO it can potentially support more code though, by using camlp4.
 *  - ocamldoc with graphviz  
 *    graphviz does not do variable node size for free as in gephi
 *  - ocamldoc -dot-reduce ... 
 *    The -dot-reduce is good for layering, but sometimes
 *    it's also good to see things without the reduction (especially
 *    with gephi). See for instance the graph for tiger with ocamldoc
 *    vs pm_depend. I can see all the real callers to option.ml.
 *    TODO the reduce and layering is also useful
 *  - ocamldoc -dot-colors
 *    TODO this is useful.
 *    It's somehow covered by the strongly-connected + coloring in gephi.
 *  - graphviz backend? graphviz is good for layers, but
 *    you lose space because the high-level stuff is at the top but alone.
 *    With gephi/fatlas, by putting high-level stuff at the center, 
 *    you lose less space? Also graphviz does not scale very well.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let log_level = ref (Some Logs.Warning)

let deps_style = ref DM.DepsInOut

(* you can also put this in your ~/gtkrc-2.0
 * gtk-icon-theme-name = "Tango"
 * gtk-theme-name = "Murrine-Gray"
 * gtk-font-name = "DejaVu Sans 16"
 *)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let constraints_of_info_txt info_txt =
  let h = Hashtbl.create 101 in
  (* pr2_gen info_txt; *)
  let rec aux current node =
    match node with
    | Common2_.Tree (node, xs) ->
      let title = node.Outline.title in
      let entry = 
        match title with
        | "__ROOT__" -> "."
        | _ -> Filename.concat current title
      in
      let children = xs |> List.map (fun (Common2_.Tree (node, _)) ->
        (match entry with
        | "." -> node.Outline.title
        | _ -> Filename.concat entry node.Outline.title
        )
      )
      in
      if not (List_.null children) then begin
        (* pr2_gen (entry, children); *)
        Hashtbl.add h entry children;
        let new_current =
          match entry with
          | "." -> ""
          | _ -> entry
        in
        List.iter (aux new_current) xs
      end
  in
  aux "" info_txt;
  h

(*****************************************************************************)
(* Model Helpers *)
(*****************************************************************************)

let dep_file_of_dir dir = 
  Filename.concat dir !!(Graph_code.default_filename)

let build_model (root : Fpath.t) =
  let file = dep_file_of_dir !!root in
  let g = Graph_code.load (Fpath.v file) in
  let gopti = 
    Cache_disk.cache_computation file ".opti"
      (fun () -> Graph_code_opti.convert g)
  in
  (* todo: find -name "info.txt" *)
  let constraints =
    if Sys.file_exists (Filename.concat !!root "info.txt")
    then begin
      UCommon.pr2 (spf "using %s" (Filename.concat !!root "info.txt"));
      let info_txt = Info_code.load (Filename.concat !!root "info.txt") in
      constraints_of_info_txt info_txt
    end
    else Hashtbl.create 0
  in
  { Model.g_deprecated = g; gopti = gopti; 
    root;
    constraints 
  }

let dir_node xs =     
  (String.concat "/" xs, Entity_code.Dir)
let package_node xs = 
  (String.concat "." xs, Entity_code.Package)

(*****************************************************************************)
(* Main action, viewing the graph *)
(*****************************************************************************)

(* algo: 
 *  - find root of project with a dependencies.marshall file
 *  - display slice if needed of the dependency hieararchical matrix 
 *    using arguments in xs.
 * No need of -with_extern anymore, external stuff will be collapsed.
 * No need of -package_depth, can expand on demand after.
 * 
 * todo: How load graph? Build on demand? easier to test things that way ... 
 * maybe can just cache and look if we need to recompute the code graph?
 * same for the dependency matrix that we can cache too.
 *)
let main_action (x : string) =
  let _locale = GtkMain.Main.init () in

  let dir : string (* TODO Fpath.t *) = 
    Rpath.canonical_exn (Fpath.v x) |> Fpath.to_string
  in
  (* this is to allow to use codegraph from a subdir, see the comment
   * below about "slice of the graph"
   *)
  let inits = Common2_.inits_of_absolute_dir dir in
  let root : Fpath.t =
    try 
      inits |> List.rev |> List.find (fun path -> 
        Sys.file_exists (dep_file_of_dir path)) |> Fpath.v
    with Not_found ->
      failwith (spf "could not find the codegraph file from %s or its parent"
                  dir);
  in
  (* let file = dep_file_of_dir root in *)
  Logs.info (fun m -> m "Using root = %s" !!root);
  let model = build_model root in

  let path =
    if Fpath.equal root (Fpath.v dir)
    then []
    else begin
      (* Propose a specific slice of the graph.
       * If run cg from a/b/c, then Expand a, Expand a/b,
       * then Focus a/b/c, and optionally Expand a/b/c.
       *)
      let readable_subdir =
        let xs = String_.split ~sep:"/" !!root in
        let ys = String_.split ~sep:"/" dir in
        let (a, b) = Common2_.splitAt (List.length xs) ys in
        assert (xs =*= a);
        b
      in
      let dir_or_package, start =
        if GC.has_node (dir_node readable_subdir) model.Model.g_deprecated
        then dir_node, readable_subdir
        else package_node, 
              try
               Common2_.tails readable_subdir |> List.find (fun xs ->
                 GC.has_node (package_node xs) model.Model.g_deprecated
               )
              with Not_found ->
                failwith "could not find a Dir or Package"
      in
      let (str, kind) = dir_or_package start in
      UCommon.pr2 (spf "focusing on %s %s" 
              (Entity_code.string_of_entity_kind kind) str);
      let rec aux before xs =
        match xs with
        | [] -> raise Impossible
        | [x] ->
            let node = dir_or_package (List.rev (x::before)) in
            [DM.Focus (node, !deps_style); DM.Expand node;]
        | x::xs ->
            let node = dir_or_package (List.rev (x::before)) in
            (DM.Expand node)::aux (x::before) xs
      in
      (aux [] start)
    end
  in

  let w = Model.init_world path model in
  View.mk_gui w

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
let extra_actions () = [
(*
  "-test_phylomel", " <geno file>",
  Common.mk_action_1_arg test_phylomel;
*)
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions () @
  (* Layer_graph_code.actions() @ *)
  (*  Test_program_lang.actions() @ *)
  []

let options () = [
  "-deps", Arg.String (fun s -> 
    deps_style := 
      (match s with
      | "in" -> DM.DepsIn
      | "out" -> DM.DepsOut
      | "inout" -> DM.DepsInOut
      | _ -> failwith "wrong parameter for --deps"
      )
  ), 
  " <in|out|inout>";

  "-dots_threshold", Arg.Int (fun i -> DMBuild.threshold_pack := i),
  " <int> when do we introduce '...' entries";

  ] @
  Arg_.options_of_actions action (all_actions()) @
  Common2_.cmdline_flags_devel () @
  [
  "-verbose", Arg.Unit (fun () ->
    log_level := Some (Logs.Info);
    DM.verbose := true;
  ), " ";
  "-debug", Arg.Unit (fun () -> log_level := Some (Logs.Debug)), " ";
  "-quiet", Arg.Unit (fun () -> log_level := None), " ";
  "-version",   Arg.Unit (fun () -> 
      UCommon.pr2 (spf "CodeGraph version: %s" "TODO: codegraph version");
      exit 0;
    ), 
   " guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  (* Common_extra.set_link(); *)
  let usage_msg = 
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codegraph"
  in
  
  (* does side effect on many global flags *)
  let args = Arg_.parse_options (options()) usage_msg Sys.argv in

  (* alt: use cmdliner and parse --debug, --info ... *)
  Logs_.setup ~level:!log_level ();
  Logs.info (fun m -> m "Starting logging");

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () -> 

    (match args with
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Arg_.action_list (all_actions())) -> 
        Arg_.do_action !action xs (all_actions())

    | _ when not (String_.empty !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | [ x ]  -> 
        main_action x

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] | _::_::_ -> 
        Arg_.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  UCommon.main_boilerplate (fun () -> 
    main ();
  )
