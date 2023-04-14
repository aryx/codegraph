(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c, All rights reserved.
 *
 *)
open Common
module AST = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Resolved names!
 *
 * Here we want to maintain the strong invariants that all the names
 * are fully qualified and "resolved".
 *
 * Related types: AST_generic.name, Graph_code.entname.
 * alternative filenames: Name_AST.ml, Final_name.ml
 * 
 * TODO: rename the converters dotted_ident_of_x to just of_x or to_x
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type qualifier = AST_generic.dotted_ident
[@@deriving show]

(* TODO? at some point we may want to separate the qualifier
 * from the final part and so have t != qualifier.
 * TODO: handle generics? reuse AST_generic.qualified_info?
 *)
type t = AST_generic.dotted_ident
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* When we create a node, we need to qualify it fully, because each
 * node must be unique (no duplicate nodes) *)
let to_entname xs = xs |> List.map fst |> Common.join "."

(* When we lookup things, we actually care only about the last part
 * of the name as we gradually go down in the graph.
 *)
let dotted_ident_of_entname str = Common.split "\\." str

let dotted_ident_of_dir str = Common.split "/" str

(* see also AST_generic_helpers.name_of_ids *)

let last_ident xs =
  match List.rev xs with
  | [] -> raise Impossible
  | x :: _ -> x

let of_ast_name = function
  | AST.Id ((s, tok), v2) -> (
      match !(v2.id_resolved) with
      | Some (AST.ImportedEntity xs, _sid)
      | Some (AST.ImportedModule (xs), _sid)
      (* This can actually introduces some regressions, take care! *)
      | Some (AST.GlobalName (xs, []), _sid)
      ->
          Some (xs |> Common.map (fun x -> x, tok))
      | Some _ -> 
            logger#info "of_ast_name: %s, no resolvable name (Some _)" s; 
            None
      (* this can now be set in L.lookup_name_and_set_resolved_if_needed *)
      | None -> 
            logger#info "of_ast_name: %s, no resolvable name (None)" s;
            None)
  (* TODO *)
  | AST.IdQualified _ -> 
      logger#info "of_ast_name: No resolvable name found (TODO IdQualified)";
      None

(* todo: generate an Id with id_resolved or directly an IdQualified?
 * What will work in typed_metavar m_compatible_type?
 *)
let to_ast_name (xs : AST.dotted_ident) : AST.name = 
  AST_generic_helpers.name_of_ids xs

(*****************************************************************************)
(* Resolved *)
(*****************************************************************************)

(* To use when Naming_AST.ml was not able to resolve *)
let set_resolved_if_none name xs =
  match name with
  | AST.Id (_v1, v2) -> (
        match !(v2.id_resolved) with
        | None -> v2.id_resolved := (Some (GlobalName (xs, []), AST.SId.unsafe_default)) ;
        | _ -> ())
  | _ -> ()
