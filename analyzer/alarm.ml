open Misc

let report loc r_opt =
  if !set_verbose then
    match r_opt with
    | Error _ ->
      Caml.Format.eprintf "%aTyping error.@."
        Location.output_location loc;
      raise Stdlib.Exit
    | Ok _ -> r_opt
  else r_opt
