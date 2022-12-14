open Ibase.Monad
open Result
open Ast
open Alarm
open AbstractValue

module Make(D: AbstractDomain.S) = struct

  let rec iexp genv env { e_desc; e_loc } =
    match e_desc with
    | Econst _ | Econstr0 _ | Elocal _ | Eglobal _ | Elast _ ->
      failwith "error stateless"
    | Eop(op, e_list) ->
      begin match op, e_list with
        | Efby, [{ e_desc = Econst(v) }; e] ->
          let s = iexp genv env e in
          assert false
        | _ -> failwith "error op"
      end
    | _ -> assert false

  and ieq genv env { eq_desc; eq_loc }: ('a, 'b) Result.t =
    match eq_desc with
    | EQeq(_, e) -> iexp genv env e
    | _ -> assert false

  and ivardec genv env { var_default; var_loc } =
    match var_default with
    | Ewith_init(e) ->
      assert false
    | Ewith_default(e) -> iexp genv env e
    | Ewith_nothing -> assert false
    | Ewith_last -> assert false

  let declare_arg env { var_name; var_default } exp =
    D.assign [var_name] exp env

  let remove_arg env { var_name; var_default } =
    D.remove [var_name] env

  let mk_return env f_res f_loc =
    let rec aux env out_l return_l =
      match out_l, return_l with
      | [], _ -> return env
      | _, [] -> fail Generic_alarm
      | arg :: out_l, return_ident :: return_l ->
        let* env = D.assign [return_ident] { e_desc= Elocal arg.var_name; e_loc= arg.var_loc } env in
        aux env out_l return_l
    in
    aux env f_res return_idents

  let funexp genv { f_kind; f_atomic; f_args; f_res; f_body; f_loc } =
    (* ieq should return a function from the denv to another env *)
    let* si = ieq genv D.top f_body in
    let f = match f_kind with
      | Efun ->
        (* combinatorial function *)
        return
          (ACoFun
             (fun env exp_list ->
                let* env =
                  Result.fold2
                    Invalid_args_size
                    declare_arg
                    env f_args exp_list
                in
                (* TODO: seq *)
                let* env = mk_return env f_res f_loc in
                let* env =
                  List.fold_result
                    ~init:env
                    ~f:remove_arg
                    f_res
                in
                return env
             ))
      | Enode ->
        (* stateful function *)
        let* s_f_args = List.map_result ~f:(ivardec genv) f_args in
        let* s_f_res = List.map_result ~f:(ivardec genv) f_res in
        return
          (ACoNode
             {
               init =
                 (fun env ->
                    fail Generic_alarm
                 );
               step =
                 (fun s env exp_list ->
                    fail Generic_alarm
                 )
             })
    in
    report f_loc f

  let init denv i_list =
    let f denv { desc; loc} =
      match desc with
      | Eletdecl(f, e) ->
        let* si = iexp denv D.top e in
        assert false
      | Eletfundecl(f, fd) ->
        let fv = funexp denv fd in
        assert false
      | Etypedecl(f, td) ->
        return denv
    in
    List.fold_result ~f ~init:denv i_list

  let run value prog ff =
    assert false


end
