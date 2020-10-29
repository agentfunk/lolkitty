type command_builder = { bindings : string list; build : string list -> string }

val resolve_builder : string -> command_builder
