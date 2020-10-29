type command_builder = {
  desc : string;
  bindings : string list;
  build : string list -> string;
}

let google_builder =
  {
    desc = "Google";
    bindings = [ "gm"; "gmail" ];
    build = (fun _ -> "https://gmail.com");
  }

let gmail_builder =
  {
    desc = "Gmail";
    bindings = [ "g"; "goog"; "gg"; "google" ];
    build =
      (fun params ->
        let param_str = String.concat " " params in
        "https://www.google.com/search?q=" ^ param_str);
  }

let bing_builder =
  {
    desc = "Bing";
    bindings = [ "b"; "bing" ];
    build =
      (fun params ->
        let param_str = String.concat " " params in
        "https://www.bing.com/search?q=" ^ param_str);
  }

let bindings = [ google_builder; gmail_builder; bing_builder ]

let default_binding = google_builder

let resolve_builder binding =
  let res builder = List.mem binding builder.bindings in
  match List.find_all res bindings with [] -> default_binding | b :: _ -> b

let command_help_str =
  let binding_str b = String.concat ", " b.bindings in
  List.fold_left
    (fun a b -> Printf.sprintf "%s%s\t%s\n" a (binding_str b) b.desc)
    "" bindings
