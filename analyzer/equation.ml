open Ast
open Alarm
open Result
(*
type el_label = int

type ast_unary_op

type ast_binary_op

type ast_const =
  | Ast_Int of int
  | Ast_Bool of bool
  | Ast_Void

type ast_exp =
  | Ast_Const of ast_const
  | Ast_Local of Lident.t
  | Ast_UnOp of ast_unary_op * ast_exp
  | Ast_BinOp of ast_binary_op * ast_exp * ast_exp
  | Ast_Tuple of ast_exp list

type eq_exp =
  | Eq_Label of el_label
  | Eq_Union of eq_exp
  | Eq_Guard of eq_exp
  | Eq_Assign of eq_exp

type eq =
  {
    eq_handler: el_label;
    eq_exp: eq_exp
  }

type eq_trans =
  {
    mutable label_counter: int
  }

let default_eq_trans () =
  { label_counter= 0 }

let fresh_label eq_trans =
  let id = eq_trans.label_counter + 1 in
  eq_trans.label_counter <- id;
  id

let rec generate_funexp eq_trans { f_kind; f_atomic; f_args; f_res; f_body; f_loc } args_names =
  (* we verify that each input and output variables are already defined in the map *)
  let valid_signature =
    let variables = List.concat_no_order [f_args; f_res] in
    let results =
      List.map
        ~f:
          (fun el ->
             if not (Map.mem args_names el.var_name) then
               alarm Invalid_args_size f_loc
             else
               return ()
          )
        variables
    in
    all_unit results
  in
  valid_signature >>=
  (fun _ -> generate_eq eq_trans f_body args_names)

and generate_eq eq_trans { eq_desc; eq_write; eq_loc } vars_id =
  (* we shadow the variables which are already defined in the map, but it is intended !*)
  let vars_id =
    S.fold
      (fun el acc ->
         let data = fresh_label eq_trans in
         Map.set acc ~key:el ~data
      )
      eq_write
      vars_id
  in
  match eq_desc with
  | EQeq (pateq, exp) -> assert false
  | _ -> assert false

let generate { desc; loc } = match desc with
  | Eletdecl (_, _) -> assert false
  | Eletfundecl (name, funexp) ->
    if List.is_empty funexp.f_args then
      alarm (Invalid_main name) loc
    else
      let eq_trans = default_eq_trans () in
      let vars_id =
        let list =
          List.map
            ~f:(fun el -> (el.var_name, fresh_label eq_trans))
            funexp.f_res
        in
        Map.of_alist_exn (module Ident.M) list
      in
      generate_funexp eq_trans funexp vars_id
  | Etypedecl (_, _) -> return []
*)
