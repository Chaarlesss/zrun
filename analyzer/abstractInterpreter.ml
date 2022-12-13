open Result
open Ast
open Alarm

module Make(D: AbstractDomain.S) = struct

  (* generate set of equation used to in compute the fixpoint of the program in the abstract domain *)
  let init value i_list =
    let f value impl =
      let r = Equation.generate impl in
      report r
    in
    List.fold_result ~init:value ~f i_list

  let run value prog ff =
    assert false


end
