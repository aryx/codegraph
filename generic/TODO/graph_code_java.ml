(*
 * choices:
 *  - package-based or dir-based schema? Seems simpler to use packages.
 *  - merge overloaded methods? yes, alternative is to mangle the
 *    name of the method with the type (a la C++ linker)
 *
 * schema:
 *   Package -> SubPackage -> Class (TODO | Interface )
 *                                    -> Method
 *                                    -> Field
 *                                    -> Constant (static final)
 *                                    -> Constant (enum, inlined in parent)
 *                            Class -> SubClass -> ...
 *                                          -> EnumSubClass (nothing)
 *   (when have no package)
 *   Dir -> Subdir -> File -> Class
 *
 *   PB -> Not_Found -> Package2 -> SubPackage2 -> ...
 *
 * note: adjust graph to remove intermediate singleton? com.xxx? Hmm better
 * to do that lazily in codegraph itself.
 *
 * note: doing codegraph for Java helps evaluate the number of lookup failures
 * in projects, and which code one needs to include to fully analyze the code.
 * If I go in the abstract interpreter path that julien took where he analyzed
 * code but had so many Not_found, Todo, exn, then I'll have no confidence
 * at all. So:
 *
 * - DONE lookup package correctly
 * - SEMI lookup classes correctly
 * - lookup field/methods correctly
 *
 * It also helps to find bug in the parser and better understand
 * Java and the AST :) e.g. Name -> Dot ambiguities.
 * It also helps to see which code is needed to fully analyze our code.
 *
 *)

type env = {
  (* import x.y.* => [["x";"y"]; ...] *)
  imported_namespace: (string list) list;
  (* import x.y.z => [("z", (false, ["x";"y";"z"])); ...] *)
  imported_qualified: (string * (bool * Ast_java.qualified_ident)) list;

  (* This field is to avoid looking up parameters or locals in the graph.
   * We could also store them in the code graph so that the lookup
   * would work, but really fine-grained intra-method dependencies
   * are not that useful.
   *
   * The boolean final is because such locals/parameters should be
   * passed to anonymouse classes.
  *)
  params_or_locals: (string * bool (* is_final *)) list;
  (* To avoid looking up type parameters in the graph. *)
  type_parameters: string list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name xs =
  xs |> List.map (fun (_tyarg_todo, ident) -> Ast.unwrap ident) |>
  Common.join "."

(* helper to build entries in env.params_or_locals *)
let p_or_l v =
  Ast.unwrap v.name, Ast.is_final v.mods


let looks_like_class_name s =
  s =~ "[A-Z]"
let _looks_like_enum_constant s =
  s =~ "^[A-Z_0-9]+$"

let rec classname_and_info_of_typ t =
  match t with
  | TBasic x -> x
  | TArray (_, t, _) -> classname_and_info_of_typ t
  | TClass xs ->
      let x = Common2.list_last xs in
      let (ident, _args) = x in
      ident

(*****************************************************************************)
(* Class/Package Lookup *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101

let lookup_fully_qualified_memoized env x =
  Common.profile_code "Graph_java.lookup_qualified" (fun () ->
    if env.phase = Uses || env.phase = Inheritance
    then
      Common.memoized _hmemo x (fun () ->
        Package_java.lookup_fully_qualified2 env.g x
      )
    else Package_java.lookup_fully_qualified2 env.g x
  )

(* Java allows to open namespaces by for instance importing packages
 * in which case we unsugar by preprending the package name.
 * Note that extending a class also imports its namespace (and
 * of all its parents too), hence import_of_inherited_classes below.
*)
let with_full_qualifier env xs =
  env.imported_namespace |> List.map (fun (qualified_ident) ->
    let rev = List.rev qualified_ident in
    let prefix =
      (* todo: simplify now that have imported_qualified? *)
      match rev with
      | ("*")::rest ->
          List.rev rest
      (* todo opti: if head match the head of xs, then can accelerate things? *)
      | _ -> List.rev (List.tl rev)
    in
    prefix @ (xs |> List.map Ast.unwrap)
  )

(* Look for entity (package/class/method/field) in list of imported
 * packages or in global scope. Return fully qualified entity.
 *
 * Note that the code graph store nodes in fully qualified form.
*)
let (lookup2: env -> Ast.qualified_ident -> Graph_code.node option) =
  fun env xs ->
  let candidates = with_full_qualifier env xs in
  (* pr2_gen candidates; *)
  candidates |> Common.find_some_opt (fun full_qualifier ->
    lookup_fully_qualified_memoized env full_qualifier
  )

let lookup a b =
  Common.profile_code "Graph_java.lookup" (fun () -> lookup2 a b)

(* pre: the Inheritance phase must have been done already
 * otherwise parents_inheritance can be empty or incomplete.
*)
let rec import_of_inherited_classes env n =
  (* A class should Use only entities its extends or implements.
   * less: could filter out interface but needs them to store
   * then as E.Class E.Interface
  *)
  let parents_inheritance = G.succ n G.Use env.g in
  parents_inheritance |> Common.map_filter (fun (str, kind) ->
    match kind with
    | E.Class ->
        let xs = (Common.split "\\." str) @ ["*"] in
        let res = import_of_inherited_classes env (str, kind) in
        Some (xs::res)
    | _ -> None
  ) |> List.flatten

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
(* Note that there is no ~dupe argument. Java code uses packages and
 * fully qualified entities so there should be no name conflicts.
*)
let rec extract_defs_uses ~phase ~g ~ast ~readable ~lookup_fails =
  ignore(lookup_fails);

  let env = {
    current =
      (match ast with
       | (DirectiveStmt (Package (_, long_ident, _)))::_ ->
           (str_of_qualified_ident long_ident, E.Package)
       | _ ->            (readable, E.File)
      );
    current_qualifier =
      (match ast with
       | (DirectiveStmt (Package (_, long_ident, _)))::_ -> long_ident
       | _ -> []
      );
    imported_namespace =
      (match ast with
       (* we automatically import the current.package.* *)
       | (DirectiveStmt (Package (_, long_ident,_ )))::_ ->
           [List.map Ast.unwrap long_ident @ ["*"]]
       | _ -> []
      ) @
      ((ast |> Common.map_filter (function
         | DirectiveStmt (Import (_is_static, _import)) ->
             (* List.map Ast.unwrap qualified_ident *) raise Todo
         | _ -> None
       )) @ [
         (* we automatically import java.lang.* *)
         ["java";"lang";"*"];
         (* we automatically import top packages *)
         ["*"]
       ]
      );
    imported_qualified = ast |> Common.map_filter (function
      | DirectiveStmt (Import (_is_static, _import)) ->
          raise Todo
      | _ -> None
(*
      match List.rev xs with
      | [] -> raise Impossible
      | ["*", _] -> None
      | (s, _)::_rest -> Some (s, (is_static, xs))
*)
    );
  }
  in

  if phase = Defs then begin
    match ast with
    | (DirectiveStmt (Package (_, long_ident, _)))::_ ->
        create_intermediate_packages_if_not_present g G.root long_ident;
        (* have None usually for scripts, tests, or entry points *)
    | _ -> xxx
  end;
  (* double check if we can find some of the imports
   * (especially useful when have a better java_stdlib/ to report
   * third-party packages not-yet handled).
  *)
  if phase = Inheritance then begin
    ast |> List.iter (function
      | DirectiveStmt (Import (_is_static, _import)) ->
          let qualified_ident_bis =
            raise Todo
      (*
        match List.rev qualified_ident with
        | ("*",_)::rest -> List.rev rest
        (* less: just lookup the class for now *)
        | _x::xs when is_static -> List.rev xs
        | _ -> qualified_ident
      *)
          in
          let entity = List.map Ast.unwrap qualified_ident_bis in
          (match lookup_fully_qualified_memoized env entity with
           | Some _ ->
               (* no need add_use_edge here, it will be done later when
                * one use the entity
                * less: could be used to detect useless import
               *)
               ()
           | None ->
               pr2_once (spf "PB: wrong import: %s"
                           (str_of_qualified_ident qualified_ident_bis))
          )
      | _ -> ()
    );
  end;

  (* imports is not the only way to use external packages, one can
   * also just qualify the classname or static method so we need
   * to visit the AST and lookup classnames (possibly using information
   * from the import to know where to look for first).
  *)
  stmts env ast

(* ---------------------------------------------------------------------- *)
(* Declarations (classes, fields, etc) *)
(* ---------------------------------------------------------------------- *)
and decl env = function
  | Init (_is_static, st), n ->
      let name = spf "__init__%d" n in
      let full_ident = env.current_qualifier @ [name, fakeInfo name] in
      let full_str = str_of_qualified_ident full_ident in
      let node = (full_str, E.TopStmts) in
      if env.phase = Defs then begin
        env.g |> G.add_node node;
        env.g |> G.add_edge (env.current, node) G.Has;
      end;
      let env = { env with
                  current = node;
                  current_qualifier = full_ident;
                }
      in
      stmt env st

and decls env xs = List.iter (decl env) (Common.index_list_1 xs)

and class_decl env def =
  let full_ident = env.current_qualifier @ [def.cl_name] in
  let full_str = str_of_qualified_ident full_ident in
  let node = (full_str, E.Class) in
  if env.phase = Defs then begin
    (* less: def.c_type? *)
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.cl_name);
    env.g |> G.add_edge (env.current, node) G.Has;
  end;
  let env = { env with
              current = node;
              current_qualifier = full_ident;
              (* with anon classes we need to lookup enclosing final parameters/locals *)
              params_or_locals = env.params_or_locals |> List.filter (fun (_x,b) -> b);
              type_parameters = def.cl_tparams |> List.map (function
                | TParamEllipsis _ -> raise Impossible
                | TParam ((str,_tok), _constraints) -> str
              );
            }
  in
  let parents =
    Common2.option_to_list def.cl_extends @
    (def.cl_impls)
  in
  List.iter (typ env) parents;

  let imports =
    if env.phase = Defs then []
    else
      (* Java allows programmer to use fields without qualifying them
       * (without a class.xxx, or this.xxx) so we need to unsugar this
       * by prepending the full current classname. We can just
       * generate a fake import package.classname.*. This will also
       * allow nested classes to access siblings.
      *)
      (List.map Ast.unwrap full_ident @ ["*"]) ::
      import_of_inherited_classes env (full_str, E.Class)
  in
  decls {env with imported_namespace = imports @ env.imported_namespace }
    (unbracket def.cl_body)

(* Java allow some forms of overloading, so the same method name can be
 * used multiple times.
*)
and method_decl env def =
  let env = { env with
              current = node;
              (* No change to the qualifier? methods are not a namespace?
               * Hmm but can have nested classes inside a methods that
               * share the same name so yes need full_ident as a qualifier.
              *)
              current_qualifier = full_ident;
              params_or_locals =
                (def.m_formals |> Common.map_filter (function
                   | ParamClassic p | ParamReceiver p | ParamSpread (_, p) -> Some p
                   | ParamEllipsis _ -> None)
                 |> List.map p_or_l)
                @
                (* with methods of anon classes we need to lookup enclosing
                 * final parameters/locals
                *)
                (env.params_or_locals |> List.filter (fun (_x,b) -> b));

              (* TODO use m_tparams *)
              type_parameters = [];
            }
  in
  xxx

and field_decl env def =
  let full_ident = env.current_qualifier @ [def.f_var.name] in
  let full_str = str_of_qualified_ident full_ident in
  let kind =
    if Ast.is_final_static def.f_var.mods
    then E.Constant
    else E.Field
  in
  let node = (full_str, kind) in
  if env.phase = Defs then begin
    (* less: static? *)
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.f_var.name);
    env.g |> G.add_edge (env.current, node) G.Has;
  end;
  let env = { env with
              current = node;
              current_qualifier = env.current_qualifier
            }
  in
  field env def

and enum_decl env def =
  let full_ident = env.current_qualifier @ [def.en_name] in
  let full_str = str_of_qualified_ident full_ident in
  (* less: make it a class? or a Type? *)
  let node = (full_str, E.Class) in
  if env.phase = Defs then begin
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.en_name);
    env.g |> G.add_edge (env.current, node) G.Has;
  end;
  let env = { env with
              current = node;
              current_qualifier = full_ident;
              params_or_locals = [];
              (* TODO *)
              type_parameters = [];
            }
  in
  let parents = (def.en_impls) in
  List.iter (typ env) parents;
  let (csts, xs) = def.en_body in
  decls env xs;

  csts |> List.iter (fun (ident, args_opt, body_opt) ->
    let full_ident = env.current_qualifier @ [ident] in
    let full_str = str_of_qualified_ident full_ident in
    let node = (full_str, E.Constant) in
    if env.phase = Defs then begin
      env.g |> G.add_node node;
      env.g |> G.add_nodeinfo node (nodeinfo ident);
      env.g |> G.add_edge (env.current, node) G.Has;
    end;
    let env = { env with
                current = node;
                current_qualifier = full_ident;
              }
    in
    args_opt |> Common.do_option (fun (_, xs, _) -> exprs env xs);
    body_opt |> Common.do_option (fun (_, xs, _) -> decls env xs);
  )

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
(* mostly boilerplate, control constructs don't introduce entities *)
and stmt env = function
  | For (_, x, st) ->
      let env =
        match x with
        | ForEllipsis _t -> env
        | Foreach (v, e) ->
            var env v;
            expr env e;
            { env with
              params_or_locals = p_or_l v :: env.params_or_locals;
            }

        | ForClassic (init, es1, es2) ->
            (match init with
             | ForInitExprs es0 ->
                 exprs env (es0 @ es1 @ es2);
                 env
             | ForInitVars xs ->
                 List.iter (field env) xs;
                 let env = { env with
                             params_or_locals =
                               (xs |> List.map (fun fld -> p_or_l fld.f_var)
                               ) @ env.params_or_locals;
                           }
                 in
                 exprs env (es1 @ es2);
                 env
            )
      in
      stmt env st;

and stmts env xs =
  let rec aux env = function
    | [] -> ()
    | x::xs ->
        stmt env x;
        let env =
          match x with
          | LocalVar fld ->
              { env with
                params_or_locals = p_or_l fld.f_var :: env.params_or_locals }
          (* also add LocalClass case? no, 'lookup env ...' handles that *)
          | _ -> env
        in
        aux env xs
  in
  aux env xs


and catch env (_, catch_exn, st) =
  match catch_exn with
  | CatchParam (v, _uniontypes) ->
      var env v;
      let env = { env with params_or_locals = p_or_l v :: env.params_or_locals } in
      stmt env st

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  (* main dependency source! *)
  | This _ -> ()
  | NameId _ -> failwith "TODO: look Name below"
(*
  | Name n ->
      if env.phase = Uses then begin
        let str = str_of_name n in
        (match str, n with
         (* TODO: look at the type and continue lookup *)
         | _, (_,(s,_))::_rest when List.mem_assoc s env.params_or_locals -> ()
         (* TODO *)
         | "super", _ | "this", _ ->
             ()
         | _ ->
             (match lookup env (long_ident_of_name n) with
              | Some n2 ->
                  add_use_edge env n2
              | None ->
                  (match n with
                   | [] ->
                       pr2 "Name is empty??";
                       pr2_gen (env.current, n);
                       raise Impossible
                   | (_, (s,_))::_ when List.mem_assoc s env.imported_qualified ->
                       let (_is_static, full_ident) =
                         List.assoc s env.imported_qualified in
                       let str = str_of_qualified_ident full_ident in
                       add_use_edge env (str, E.Package)

                   | [_x] when looks_like_enum_constant str ->
                       pr2 ("PB: " ^ Common.dump n);
                   | [_x] when looks_like_class_name str ->
                       add_use_edge env (str, E.Package)
                   | [_x] ->
                       pr2 ("PB: " ^ Common.dump n);
                       (* env.imported_namespace +> List.iter pr2_gen; *)
                   | _x::_y::_xs ->
                       (* unknown package probably *)
                       add_use_edge env (str, E.Package)
                  )
             )
        )
      end
*)
  | NewClass (tok, t, (_, args, _), decls_opt) ->
      typ env t;
      exprs env args;
      (match decls_opt with
       | None -> ()
       | Some xs ->
           (* less: quite similar to class_decl, factorize code? *)
           let classname, info  = classname_and_info_of_typ t in
           let charpos = PI.pos_of_info info in
           let anon_class = spf "__anon__%s__%d" classname charpos in
           let cdecl = {
             cl_name = (anon_class, info);
             cl_extends = Some t;
             cl_impls = [];
             cl_kind = (ClassRegular, tok) ;
             cl_body = xs;
             (* ?? *)
             cl_tparams = [];
             cl_mods = [];
             cl_formals = [];
           }
           in
           class_decl env cdecl
      )
  | NewQualifiedClass (_e, _t1, tok, ty, (args), decls_opt) ->
      (*
      pr2 "NewQualifiedClass";
      pr2_gen (NewQualifiedClass (e, id, args, decls_opt));
      *)
      (* todo: need to resolve the type of 'e' *)
      expr env (NewClass (tok, ty, args, decls_opt))

  | Dot (e, _t, _idTODO) ->
      (* todo: match e, and try lookup method/field
       * if e is a Name, lookup it, and if a class then
       * lookup children. If local ... then need get its type
       * lookup its node, and then lookup children.
      *)
      expr env e;
(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
and typ env = function
  | TBasic _ -> ()
  | TArray (_, t, _) -> typ env t
  (* other big dependency source! *)
  | TClass reft ->
      (* todo: let's forget generic arguments for now *)
      let xs = long_ident_of_class_type reft in
      let str = str_of_qualified_ident xs in
      if env.phase = Uses || env.phase = Inheritance then begin
        (match str, reft with
         (* TODO: look at the type and continue lookup *)
         | _, (((s,_),_))::_rest when List.mem s env.type_parameters -> ()
         | _ ->
             (match lookup env xs with
              (* TODO: look in type_params_local ! *)
              | Some n2 ->
                  (* pr2 ("FOUND: " ^ Common.dump n); *)
                  add_use_edge env n2
              | None ->
                  (match xs with
                   | [] -> raise Impossible
                   | (s,_)::_ when List.mem_assoc s env.imported_qualified ->
                       let (_is_static, full_ident) =
                         List.assoc s env.imported_qualified in
                       let str = str_of_qualified_ident full_ident in
                       add_use_edge env (str, E.Package)

                   | [_x] ->
                       if looks_like_class_name str
                       then add_use_edge env (str, E.Package)
                       else
                         pr2 ("PB: " ^ Common.dump reft);
                   | _x::_y::_xs ->
                       (* unknown package probably *)
                       add_use_edge env (str, E.Package)
                  )
             )
        )
      end
