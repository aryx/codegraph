(*
let dump_cmt_headers file =
  let info = Cmt_format.read_cmt file in
  pr2_gen info.Cmt_format.cmt_modname;
  pr2_gen info.Cmt_format.cmt_imports;
  ()
*)

let dump_cmt file =
  let info : Ast_cmt.t = Cmt_format.read_cmt file in
  match info.Cmt_format.cmt_annots with
  | Cmt_format.Implementation x ->
      let v = Meta_ast_cmt.vof_structure x in
      let str = OCaml.string_of_v v in
      (* alt: let str = Ast_cmt.show_structure x in *)
      UCommon.pr str
  | _ -> failwith "dump_cmt: Not an Implementation"

let dump_cmt2 file =
  let info : Ast_cmt.t = Cmt_format.read_cmt file in
  let str = Ast_cmt.show info in
  UCommon.pr str


let actions () = [
  "-dump_cmt", " <file>",
  Arg_.mk_action_1_arg dump_cmt;
  "-dump_cmt2", " <file>",
  Arg_.mk_action_1_arg dump_cmt2;
]
