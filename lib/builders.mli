type command_builder = {
  desc : string;
  bindings : string list;
  build : string list -> string;
}

val resolve_builder : string -> command_builder

val command_help_str : string
