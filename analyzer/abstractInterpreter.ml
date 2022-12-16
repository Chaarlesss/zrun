open Ibase.Monad
open Result
open Ast
open Alarm
open AbstractValue

module Make(D: AbstractDomain.S) = struct

  let find_gnode_opt x env =
    let v = Map.find env x in
    match v with
    | Some(Gfun(f)) -> return f
    | _ -> failwith ("unknown node : " ^ (Lident.modname x))

  let fixpoint n stop f s bot =
    let rec fixpoint n d_el =
      if n <= 0 then
        return (0, d_el)
      else
        let* d_el' = f s d_el in
        (* d_el' = f(s)(d_el) subset of d_el ? f(s) is supposed to be monotone, so if f(s)(x) <= x then x = f(s)(x) *)
        let* guard = stop d_el' d_el in
        if guard then return (n, d_el') else fixpoint (n - 1) d_el'
    in
    fixpoint n bot

  (* find a solution to the set of equation *)
  (* be carefull with this fixpoint solver: it assumes that the equations is well formed in the concrete *)
  let fixpoint_eq genv d_el sem eq n s_eq bot =
    let sem s_eq d_el_in =
      let* d_el = D.join d_el_in d_el in
      (* j'ai pas tout compris avec le complete_with_default *)
      sem genv d_el eq s_eq
    in
    (* TODO: add some debug around here *)
    let* m, d_el_out = fixpoint n D.is_subset sem s_eq bot in
    return d_el_out

  let rec iexp genv { e_desc; e_loc } =
    let r = match e_desc with
      | Econst _ | Econstr0 _ | Elocal _ | Eglobal _ | Elast _ ->
        return
          (fun env -> return (env, ASempty))
      | Econstr1(_, e_list) ->
        let* s_list = List.map_result ~f:(iexp genv) e_list in
        return
          (fun env ->
             let* env, l =
               List.fold_result
                 ~f:(fun (env, l) s ->
                     let* env, h = s env in
                     return (env, h :: l)
                   )
                 ~init:(env, []) s_list
             in
             let l = List.rev l in
             return (env, AStuple l)
          )
      | Eop(op, e_list) ->
        begin
          match op, e_list with
          | Efby, [{ e_desc= Econst(v) } as e; e_l] ->
            let* s = iexp genv e_l in
            return
              (fun env ->
                 let* env, s = s env in
                 let init_fby_var = create_var "fby" in
                 let* env = D.assign [init_fby_var] e env in
                 return
                   (env, AStuple [ASval (AVident init_fby_var); s])
              )
          | _ -> fail Generic_alarm
        end
      | Eget(_, e) ->
        let* s = iexp genv e in
        return (fun env -> s env)
      | Eapp(f, e_list) ->
        let* s_list = List.map_result ~f:(iexp genv) e_list in
        let* v = find_gnode_opt f genv in
        let* s =
          match v with
          | ACoFun _ ->
            return (fun env -> return (env, ASempty))
          | ACoNode { init = s } -> return s
        in
        return
          (fun env ->
             let* env, l =
               List.fold_result
                 ~f:(fun (env, l) s ->
                     let* env, h = s env in
                     return (env, h :: l)
                   )
                 ~init:(env, []) s_list
             in
             let l = List.rev l in
             let* env, s = s env in
             return (env, AStuple (s :: l))
          )
      | Elet(is_rec, eq, e) ->
        let* s_eq = ieq genv eq in
        let* se = iexp genv e in
        return
          (fun env ->
             let* env, s_eq = s_eq env in
             let* env, se = se env in
             return (env, AStuple [s_eq; se])
          )
      | _ -> fail Generic_alarm
    in
    report e_loc r

  and ieq genv { eq_desc; eq_loc }: ('a, 'b) Result.t =
    let r = match eq_desc with
      | EQeq(_, e) -> iexp genv e
      | _ -> assert false
    in
    report eq_loc r

  and ivardec genv { var_default; var_loc } =
    match var_default with
    | Ewith_init(e) ->
      assert false
    | Ewith_default(e) -> assert false
    | Ewith_nothing -> (fun env -> return (env, ASempty)) |> return
    | Ewith_last -> assert false

  let svardec genv d_el { var_name; var_default; var_loc } s e =
    assert false

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
    let* si = ieq genv f_body in
    let f = match f_kind with
      | Efun ->
        (* combinatorial function *)
        return
          (ACoFun
             (fun d_el exp_list ->
                let* d_el =
                  Result.fold2
                    Invalid_args_size
                    declare_arg
                    d_el f_args exp_list
                in
                (* TODO: seq *)
                let* d_el = mk_return d_el f_res f_loc in
                let* d_el =
                  List.fold_result
                    ~init:d_el
                    ~f:remove_arg
                    (List.append f_args f_res)
                in
                return d_el
             ))
      | Enode ->
        (* stateful function *)
        let* s_f_args = List.map_result ~f:(ivardec genv) f_args in
        let* s_f_res = List.map_result ~f:(ivardec genv) f_res in
        return
          (ACoNode
             {
               init =
                 (fun d_el ->
                    let* d_el, s_f_args =
                      List.fold_result
                        ~init:(d_el, [])
                        ~f:(fun (d_el, l) s ->
                            let* d_el, h = s d_el in
                            return (d_el, h :: l)
                          )
                        s_f_args
                    in
                    let s_f_args = List.rev s_f_args in
                    let* d_el, s_f_res =
                      List.fold_result
                        ~init:(d_el, [])
                        ~f:(fun (d_el, l) s ->
                            let* d_el, h = s d_el in
                            return (d_el, h :: l)
                          )
                        s_f_res
                    in
                    let s_f_res = List.rev s_f_res in
                    let* d_el, si = si d_el in
                    return
                      (d_el, AStuple [AStuple s_f_args; AStuple s_f_res; si])
                 );
               step =
                 (fun s d_el exp_list ->
                    match s with
                    | AStuple [AStuple s_f_args; AStuple s_f_res; s_body] ->
                      let* d_el, s_f_args =
                        List.mapfold3_result
                          ~init:d_el ~error:Generic_alarm
                          ~f:(svardec genv)
                          f_args s_f_args exp_list
                      in
                      (* initialize s_f_res depending on what is stored in the state *)
                      (*let* d_body, s_body =
                        fixpoint_eq genv d_el seq f_body n s_body
                        in*)
                      failwith "a implementer"
                    | _ -> fail Generic_alarm
                 )
             })
    in
    report f_loc f

  let init _ i_list =
    let f genv { desc; loc} =
      match desc with
      | Eletdecl(f, e) ->
        assert false
      | Eletfundecl(f, fd) ->
        let* fv = funexp genv fd in
        return (Map.set genv ~key:(Name f) ~data:(Gfun(fv)))
      | Etypedecl(f, td) ->
        return genv
    in
    let genv0 =
      let plus_fun = Gfun(ACoFun (fun (_: D.t) _ -> fail Generic_alarm)) in
      Map.singleton (module Lident.M) (Name "+") plus_fun
    in
    let genv = genv0 in
    List.fold_result ~f ~init:genv i_list

  let run_node output init step d_el n: int =
    let rec runrec s d_el i =
      if i = n then i
      else
        let v =
          let* sd_el = step s d_el [] in
          let* d_el = D.join d_el sd_el in
          let* _ = output d_el in
          return s in
        match v with
        | Error _ -> i
        | Ok s -> runrec s d_el (i+1)
    in
    match init d_el with
    | Error err -> failwith "initialization failed"
    | Ok (d_el, s) -> runrec s d_el 0

  let run genv main ff =
    let* fv = find_gnode_opt (Name main) genv in
    let output d_el =
      let _ = D.pp ff d_el in
      return ()
    in
    match fv with
    | ACoFun(fv) -> assert false
    | ACoNode { init; step } ->
      let _ = run_node output init step D.top 20 in
      return ()

end
