open Opium.Std
open Lib.Builders

type command_params = { command : string; params : string list }

let ( let+ ) x f = Option.map f x

let resolve_url = function
  | { command = c; params = p } ->
      let builder = resolve_builder c in
      builder.build p

module StringSet = Set.Make (String)

let get_command_params param_str =
  let params = param_str |> String.split_on_char ' ' in
  match params with
  | [] -> None
  | [ c ] -> Some { command = c; params = [] }
  | c :: ps -> Some { command = c; params = ps }

let get_response param_str =
  let+ cmd = get_command_params param_str in
  let url = resolve_url cmd in
  Response.redirect_to @@ Printf.sprintf "%s\n" url

let print_param =
  get "/:params" (fun req ->
      let params = Router.param req "params" in
      Lwt.return
      @@
      match get_response params with
      | None ->
          Response.of_plain_text @@ Printf.sprintf "Invalid param [%s]" params
      | Some response -> response)

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  App.empty |> print_param |> App.run_command
