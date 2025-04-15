
val paint: Model.world -> unit

val button_action:
 < as_widget : [> `widget ] Gtk.obj; .. > ->
   Model.world -> GdkEvent.Button.t -> bool

val recompute_matrix:
  Model.world -> unit
