(lang dune 2.7)
; 2.7 instead of 3.7 so dune does not generate confusing .pp.ml for codegraph
; and syncweb_indexer

(name codegraph)
(using menhir 2.0)

(generate_opam_files)

(source (github aryx/codegraph))
(homepage "https://github.com/aryx/codegraph")
(maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
(authors "Yoann Padioleau <yoann.padioleau@gmail.com>")

(package (name codegraph)
  (synopsis "Code dependency graph builder and visualizer")
  (description "\
Codegraph builds and visualizes code dependency graphs for multi-language
codebases using semgrep parsers.

For more information see https://github.com/aryx/codegraph
"
  )

  (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

  (depends
    (dune (>= 3.7))
    ; codegraph's own deps
    lablgtk
    cairo2-gtk
    ounit
    ocamlgraph
    ; External deps of vendored submodules (semgrep-pfff-libs, semgrep-pfff-langs).
    ; These are needed because the submodules are built from source as part of
    ; the dune workspace, so their opam deps are not resolved automatically.
    ; coupling: semgrep-pfff-libs/dune-project, semgrep-pfff-langs/dune-project
    ; -- from semgrep-pfff-libs (mainly commons) --
    ANSITerminal
    alcotest
    alcotest-lwt
    atdgen
    bos
    (cmdliner (and (>= "1.3.0") (< "2.0.0")))
    digestif
    fmt
    fpath
    git
    logs
    lwt
    lwt_ppx
    memtrace
    ocolor
    pcre
    pcre2
    ppx_deriving
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    ppx_sexp_conv
    ppxlib
    re
    semver
    sexplib
    testo
    timedesc
    trace
    (opentelemetry (< "0.11"))
    (opentelemetry-client-ocurl (< "0.11"))
    ambient-context-lwt
    uri
    uuidm
    yojson
    ; -- from semgrep-pfff-langs --
    ; coupling: menhir version must match what semgrep-pfff-langs parsers
    ;  were tested with — newer menhir can generate different conflict reports
    ;  and slightly different parser code (e.g., tuple shapes in actions).
    (menhir (= "20230608"))
    parmap
    tsort
    visitors
    yaml
  )
)

; codegraph's own public libraries

(package (name pfff-lang_c-analyze)
  (synopsis "C code graph analyzer")
  (description "C code dependency graph builder.")
)
(package (name pfff-lang_java-analyze)
  (synopsis "Java code graph analyzer")
  (description "Java code dependency graph builder.")
)
(package (name pfff-lang_js-analyze)
  (synopsis "JavaScript code graph analyzer")
  (description "JavaScript code dependency graph builder.")
)
(package (name pfff-lang_lisp-analyze)
  (synopsis "Lisp code graph analyzer")
  (description "Lisp code dependency graph builder.")
)
(package (name pfff-lang_ml-analyze)
  (synopsis "OCaml code graph analyzer")
  (description "OCaml code dependency graph builder.")
)
(package (name pfff_lang_php_analyze)
  (synopsis "PHP code graph analyzer")
  (description "PHP code dependency graph builder.")
)
(package (name pfff-lang_cmt-analyze)
  (synopsis "OCaml CMT code graph analyzer")
  (description "OCaml bytecode metadata code graph builder.")
)
(package (name pfff-lang_cmt)
  (synopsis "OCaml CMT parser")
  (description "OCaml bytecode metadata parser.")
)
(package (name pfff-lang_GENERIC_finding)
  (synopsis "Generic language file finder")
  (description "Find source files by language using AST_generic.")
)
(package (name pfff-lang_GENERIC_linking)
  (synopsis "Generic code linking")
  (description "Language-generic code entity linking for dependency graphs.")
)
(package (name pfff-lang_GENERIC_parsing)
  (synopsis "Generic language parsing")
  (description "Parse source files into AST_generic for multiple languages.")
)
