(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
open Graph_code_AST_env

module E = Entity_code
module G = Graph_code

module AST = AST_generic

let str_of_dotted_ident xs =
  xs |> List.map fst |> Common.join "."

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common2.inits xs |> List.map str_of_dotted_ident in
  let dirs =
    match dirs with
    | ""::xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x::xs ->
        let entity = x, E.Package in
        if G.has_node entity g
        then aux entity xs
        else begin
          g |> G.add_node entity;
          g |> G.add_edge (current, entity) G.Has;
          aux entity xs
        end
  in
  aux root dirs


let add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  (match () with
   | _ when not (G.has_node src env.g) ->
       pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));

   | _ when G.has_node dst env.g ->
       G.add_edge (src, dst) G.Use env.g

   | _ ->
       (match kind with
        | _ ->
            let kind_original = kind in
            let dst = (name, kind_original) in
            let parent_target = G.not_found in
            (match kind_original with
             | E.Package ->
                 let fake_package =
                   (Common.split "\\." name) |> List.map (fun s -> s^"2") in
                 let dst = (Common.join "." fake_package, kind_original) in
                 if not (G.has_node dst env.g)
                 then begin
                   create_intermediate_packages_if_not_present
                     env.g parent_target
                     (fake_package |> List.map (fun s -> s,()));
                   pr2 (spf "PB: lookup fail on %s (in %s)"
                          (G.string_of_node dst) (G.string_of_node src));
                 end;
                 env.g |> G.add_edge (src, dst) G.Use;
                 ()
             | _ ->
                 pr2 (spf "PB: lookup fail on %s (in %s)"
                        (G.string_of_node dst) (G.string_of_node src));
                 G.add_node dst env.g;
                 env.g |> G.add_edge (parent_target, dst) G.Has;
                 env.g |> G.add_edge (src, dst) G.Use;
            )
       )
  )
