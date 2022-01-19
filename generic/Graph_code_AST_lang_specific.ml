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
open Common

module G = Graph_code
module E = Entity_code
module PI = Parse_info
(* open Graph_code_AST_env *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Every Lang-specific code should be in this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let split_readable readable =
  let (d, b, _e) = Common2.dbe_of_filename readable in
  Common.split "/" d @ [b]

(*****************************************************************************)
(* Lang-specific helpers *)
(*****************************************************************************)

let top_parent_and_qualifier ~lang ~readable ~ast =
  match lang with
  | Lang.Python ->
      let xs = split_readable readable in
      let tk = PI.first_loc_of_file readable |> PI.mk_info_of_loc in
      let dotted_idents =
        xs |> List.map (fun s -> s, tk) in
      let node = (readable, E.File) in
      node, dotted_idents
      
  | Lang.Java ->
      ignore(ast);
      failwith "TODO"
  | l -> failwith (spf "language not handled yet: %s" (Lang.to_string l))
