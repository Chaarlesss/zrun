open Misc
open Ast
open Result

type analyzer_alarm = ..

type analyzer_alarm +=
  | Generic_alarm
  | Invalid_main of name
  | Invalid_args_size

(* we may need to provide a printer for the alarm *)
let report loc r_opt =
  if !set_verbose then
    match r_opt with
    | Error desc ->
      Caml.Format.eprintf "%aAnalyzer alarm.@."
        Location.output_location loc;
      raise Stdlib.Exit
    | Ok _ -> r_opt
  else
    r_opt
