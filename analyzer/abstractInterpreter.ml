open Result
open Ast
open Alarm

module Make(D: AbstractDomain.S) = struct

  (* generate set of equation used to in compute the fixpoint of the program in the abstract domain *)
  let init value i_list =
    let f value { desc; loc } =
      let r = match desc with
        | Eletdecl(_, _) -> assert false
        | Eletfundecl(_, _) -> assert false (* do we really need to compute something in this case? *)
        | Etypedecl(_, _) -> return value
      in
      report loc r
    in
    List.fold_result ~init:value ~f i_list

  let run value prog ff =
    assert false

end
