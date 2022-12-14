open Misc
open Ast
open Result

type analyzer_error =
  | Generic_alarm
  | Invalid_main of name
  | Invalid_args_size

let localize el loc =
  { desc= el; loc }

let report loc r_opt =
  if !set_verbose then
    match r_opt with
    | Error  desc ->
      Caml.Format.eprintf "%aTyping error.@."
        Location.output_location loc;
      raise Stdlib.Exit
    | Ok _ -> r_opt
  else r_opt
