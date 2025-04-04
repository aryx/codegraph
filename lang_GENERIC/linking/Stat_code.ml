(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Keeping track of code statistics, especially analysis failures.
 *
 * Here are examples of code stats:
 *  - parsing stats
 *    (TODO factorize code with Parse_info.parsing_stats)
 *  - naming stats with lookup failures
 *    (TODO factorize code with Graph_code.statistics)
 *
 * related:
 *  - Graph_code.statistics but more focused on locations where we fail
 *    rather than raw numbers.
 *  - Parse_info.parsing_stats
 *  - semgrep/interface/Parsing_stats.atd
 *
 * TODO:
 *  - move this module in pfff/h_program_lang/ at some point.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type stat_element = { mutable total : int; mutable good : int }

let empty_stat_element () = { total = 0; good = 0 }

(* less: could also do "type ('categ, 'failure) t = ..."
 * to make it generic in the category and failure types.
 *
 * ex: "field lookup", { total = 2; good = 1 }
 *)
type t = {
  categories : (string, stat_element) Hashtbl.t;
  failures : (string, int) Hashtbl.t;
}

let empty_stats () =
  { categories = Hashtbl.create 101; failures = Hashtbl.create 101 }

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

let lookup_category st category =
  (* less: try and return a empty_stat_element if not in? *)
  (* nosemgrep *)
  Hashtbl.find st.categories category

(*****************************************************************************)
(* Adding stats *)
(*****************************************************************************)

let add st category good_or_bad =
  let stat =
    match Hashtbl.find_opt st.categories category with
    | Some x -> x
    | None ->
        let elem = empty_stat_element () in
        Hashtbl.add st.categories category elem;
        elem
  in
  stat.total <- stat.total + 1;
  if good_or_bad then stat.good <- stat.good + 1

let add_failure st str =
  let cnt =
    match Hashtbl.find_opt st.failures str with None -> 0 | Some i -> i
  in
  Hashtbl.replace st.failures str (cnt + 1)

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let elem_to_string categ elem = spf "%s: %d/%d" categ elem.good elem.total

let to_string st =
  "categories:\n"
  ^ (st.categories |> Hashtbl_.hash_to_list
    |> List.map (fun (categ, elem) -> elem_to_string categ elem)
    |> String.concat "\n")
  ^ "\nbiggest offenders:\n"
  ^ (st.failures |> Hashtbl_.hash_to_list |> Assoc.sort_by_val_highfirst
   |> List_.take_safe 20
    |> List.map (fun (str, cnt) -> spf "%s : %d" str cnt)
    |> String.concat "\n")

let total st =
  let total = ref 0 in
  let good = ref 0 in
  let ( += ) x t = x := !x + t in
  st.categories
  |> Hashtbl.iter (fun _ elem ->
         total += elem.total;
         good += elem.good);
  { good = !good; total = !total }
