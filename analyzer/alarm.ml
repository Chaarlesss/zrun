open Misc
open Ast
open Result

type analyzer_error =
  | Invalid_main of name
  | Invalid_args_size

let alarm (error: analyzer_error) loc =
  fail { desc= error; loc }

let report r_opt =
  if !set_verbose then
    match r_opt with
    | Error { desc; loc } ->
      Caml.Format.eprintf "%aTyping error.@."
        Location.output_location loc;
      raise Stdlib.Exit
    | Ok _ -> r_opt
  else r_opt
