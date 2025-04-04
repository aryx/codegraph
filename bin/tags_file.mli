type tag = {
  tag_definition_text : string;
  tagname : string;
  line_number : int;
  (* offset of beginning of tag_definition_text, when have 0-indexed filepos *)
  byte_offset : int;
  (* only used by vim *)
  kind : Entity_code.kind;
}

(* will generate a TAGS file in the current directory *)
val generate_TAGS_file :
  Common2_.filename -> (Common2_.filename * tag list) list -> unit

(* will generate a tags file in the current directory *)
val generate_vi_tags_file :
  Common2_.filename -> (Common2_.filename * tag list) list -> unit

val add_method_tags_when_unambiguous :
  (Common2_.filename * tag list) list -> (Common2_.filename * tag list) list

(* internals *)
val mk_tag : string -> string -> int -> int -> Entity_code.kind -> tag
val string_of_tag : tag -> string
val header : string
val footer : string

(* helpers used by language taggers *)
val tag_of_info : string array -> Tok.t -> Entity_code.kind -> tag
