type name = string
type long_name = string list

val module_name_of_filename : Common2_.filename -> name
val top_module_of_node : Graph_code.node -> name
