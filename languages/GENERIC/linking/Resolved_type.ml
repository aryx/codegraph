(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 *)
open AST_generic
module AST = AST_generic
module H = AST_generic_helpers
module N = Resolved_name

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types for the typing of AST_generic.expr.
 *
 * Note that right now this module is only used by Graph_code_AST.ml
 * during name resolving as we need to remember the type of entities
 * and expressions to be able to resolve field or method access.
 *
 * Why not simply reuse AST_generic.type_ ? Because here we want to make
 * sure type names are fully resolved.
 * In the futur, we may also want to apply more complex normalizations to
 * simplify type checking or typing/name resolving.
 *
 * Lots of the functions in this module return an option, so you might
 * want to use let* when you call them.
 *
 * alt: reuse, which would remove the need for the converters?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: polymorphic types *)
type t =
  (* fully qualified name, as in graph_code, for L.lookup_dotted_ident_opt
   * less: use Graph_code.node instead?
   *)
  | TN of Resolved_name.t
  (* a few builtins *)
  | TBuiltin of string AST.wrap
  | TList of t
  | TFunction of AST.parameters (* TODO? normalize also params? *) * t
  (* todos *)
  | TTodo of AST.todo_kind
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let ( let* ) = Option.bind

(*****************************************************************************)
(* TyExpr conversion (mostly for Python) *)
(*****************************************************************************)

(* reverse of Generic_vs_generic.make_dotted
 * transform a.b.c.d, which is parsed as (((a.b).c).d), in Some [d;c;b;a]
 * precondition: Naming_AST must have been called.
 *)
let undot_expr e =
  let rec aux e =
    match e.e with
    (* TODO: Id itself can have been resolved!! so we need to
     * concatenate. See tests/python/misc_regression[12].py
     *)
    | AST.N (AST.Id (id, _)) -> Some [ id ]
    | AST.DotAccess (e, _, AST.FN (AST.Id (id, _))) ->
        let* ids = aux e in
        Some (id :: ids)
    | _ -> None
  in
  let* ids = aux e in
  Some (List.rev ids)

let of_expr_after_naming e =
  match e.e with
  | AST.N n -> Some (AST.TyN n |> AST.t)
  (* For Python we need to transform a.b.c DotAccess expr in a qualified name*)
  | AST.DotAccess (_, _, _) ->
      let* ids = undot_expr e in
      Some (AST.TyN (H.name_of_ids ids) |> AST.t)
  | _ -> None

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* reverse of of_ast_type *)
let to_ast_type t =
  let rec aux t =
    match t with
    | TN xs ->
        let n = N.to_ast_name xs in
        Some (AST.TyN n |> AST.t)
    (* right now we don't use the params in
     * Generic_vs_generic.m_compatible_type so create empty params for now
     * TODO: generate right TyFun params from fparams
     *)
    | TFunction (_fparamsTODO, t) ->
        let* ty = aux t in
        Some (AST.TyFun ([], ty) |> AST.t)
    | _ -> None
  in
  aux t

(* Note that of_ast_type() is used during the Defs phase to add type
 * information to each entity.
 * TODO: the problem is that if the type of the entity (in ty) contains
 * unresolved names (e.g., Id in the same file or package), then
 * of_ast_name will return None, but we can't do like in 
 * lookup_name_and_set_resolved_if_needed() because we are still in the
 * Defs phase! So right now we do a hack, duplicating code that is
 * in lookup_name_and_set_resolved_if_needed().
 *)
let of_ast_type file_or_package_qualifier ty =
  let rec aux ty =
    match ty.t with
    | TyN n ->
        (match N.of_ast_name n with
        | Some xs -> Some (TN xs)
        | None ->
          (* Code similar to lookup_name_and_set_resolved_if_needed *)
          (match n with
          | Id (id, id_info) ->
             (match !(id_info.id_resolved) with
             | None
             | Some (Global, _) ->
                let rn = file_or_package_qualifier @ [ id ] in
                Logs.info (fun m -> m "of_ast_type: trying to qualify %s with %s"
                     (fst id) (N.to_entname rn));

                (* let's hope, we have no way to call lookup_resolved_name()
                 * here because we are still in the Defs phase *)
                Some (TN rn)
             | _ -> None
             )
          | _ -> None
        )
       )
    (* Python uses those types, but we can't fix
     * AST_generic_helpers.type_of_expr because this would introduce
     * regressions because we need naming to be done to correctly do
     * the transformation. This is why we do the transformation later
     * here with expr_to_type_after_naming_opt.
     *)
    | TyExpr e ->
        let* ty = of_expr_after_naming e in
        aux ty
    | _ -> None
  in
  aux ty
