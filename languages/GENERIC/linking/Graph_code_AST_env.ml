(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c, All rights reserved.
 *
 *)

module E = Entity_code
module G = Graph_code
module AST = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The environment used in Graph_code_AST.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  (* this is modified by side effects *)
  g : Graph_code.t;
  stats: Stat_code.t;
  (* for resolving method calls, also modified by side effects *)
  types : (Graph_code.node, Resolved_type.t) Hashtbl.t;
  (* non-mutable fields *)
  phase : phase;
  hooks : hooks;
  readable : Common2_.filename;
  lang : Lang.t;
  (* the parent to connect to when creating new nodes *)
  current_parent : Graph_code.node;
  (* the current "scope", everthing that is enclosing the current code.
   * less: no support for functors or complex modules *)
  current_qualifier : Resolved_name.qualifier;
  (* for resolving self.foo() *)
  class_qualifier : Resolved_name.qualifier option;
  (* for resolving local names (AST.Id).
   * see lookup_name_and_set_resolved_if_needed() *)
  file_or_package_qualifier : Resolved_name.qualifier;
}

(* We need 2 phases:
 * - one to get all the definitions
 * - one to get all the Uses.
 *)
and phase = Defs | Uses

and hooks = {
  on_def_node : Graph_code.node -> AST_generic.definition -> unit;
  on_extend_edge :
    Graph_code.node ->
    Graph_code.node ->
    AST_generic.entity * AST_generic.class_definition ->
    unit;
}

(*****************************************************************************)
(* Helpers used in all Graph_code_xxx modules *)
(*****************************************************************************)

let ( let* ) o f = Option.bind o f

let ( let/ ) o f = Option.iter f o
