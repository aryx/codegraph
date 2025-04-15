type stat_element = { mutable total : int; mutable good : int }

(* ex: "field lookup", { total = 2; good = 1 } *)
type t = {
  categories : (string, stat_element) Hashtbl.t;
  failures : (string, int) Hashtbl.t;
}

val empty_stats : unit -> t
val add : t -> string (* category *) -> bool (* good_or_bad *) -> unit
val add_failure : t -> string (* failure *) -> unit
val lookup_category : t -> string -> stat_element

(* display all categories and top 20 failures *)
val to_string : t -> string
val elem_to_string : string (* prefix *) -> stat_element -> string

(* aggregate all categories *)
val total : t -> stat_element
