open Ibase.Monad
open AbstractSyntax
open AbstractValue

let with_loc desc loc: 'a Ast.localized = { desc; loc }

let transform_app lident (e_list: exp list) =
  match Lident.modname lident, e_list with
  | "+", [e1; e2] -> Some (Ebinop(Eadd, e1, e2))
  | _ -> None

let transform_operator (op: Ast.operator) (e_list: exp list) =
  match op, e_list with
  | Efby, [e1; e2] -> Ebinop(Efby, e1, e2)
  | _ -> assert false

let transform_const (const: Ast.const) =
  match const with
  | Eint(i) -> Eint(i)
  | Ebool(b) -> Ebool(b)
  | _ -> assert false

let transform_default (default: 'a Ast.default) =
  match default with
  | Ewith_nothing -> Ewith_nothing
  | _ -> assert false

let rec transform_vardec_list (var_list: Ast.exp Ast.vardec list) =
  match var_list with
  | [] -> []
  | { var_name; var_default; var_loc } :: var_list ->
    let var_default = transform_default var_default in
    let var_list = transform_vardec_list var_list in
    { var_name; var_default; var_loc } :: var_list

and transform_exp ({ e_desc; e_loc }: Ast.exp): exp * (eq -> eq) =
  let id = (fun (eq: eq) -> eq) in
  match e_desc with
  | Econst(c) ->
    let c = transform_const c in
    { e_desc= Econst(c); e_loc }, id
  | Elocal(l) -> { e_desc= Elocal(l); e_loc }, id
  | Elast(l) -> { e_desc= Elast(l); e_loc }, id
  | Eapp(f, e_list) ->
    let k, e_list =
      List.fold_map
        ~init:id
        ~f:(fun k e ->
            let e, k' = transform_exp e in
            (fun (eq: eq) -> k' (k eq)), e
          )
        e_list
    in
    (match transform_app f e_list with
     | Some(e_desc) -> { e_desc; e_loc }, k
     | None ->
       let var_name = create_var ((Lident.modname f) ^ "_app") in
       let k ({ eq_desc; eq_write; eq_loc } as eq) =
         let eq_write = S.add var_name eq_write in
         let pateq = with_loc [var_name] e_loc in
         let eq_app = { eq_desc= EQapp(pateq, f, e_list); eq_write; eq_loc= e_loc } in
         let eq_and = { eq_desc= EQand([eq_app; eq]); eq_write; eq_loc= e_loc } in
         let vardec = { var_name; var_default= Ewith_nothing; var_loc= e_loc } in
         let eq_local = { eq_desc= EQlocal([vardec], eq_and); eq_write; eq_loc= e_loc } in
         k eq_local
       in
       { e_desc= Elocal(var_name); e_loc }, k
    )
  | Eop(op, e_list) ->
    let k, e_list =
      List.fold_map
        ~init:id
        ~f:(fun k e ->
            let e, k' = transform_exp e in
            (fun (eq: eq) -> k' (k eq)), e
          )
        e_list
    in
    { e_desc= transform_operator op e_list; e_loc }, k
  | _ -> assert false

and transform_eq ({ eq_desc; eq_write; eq_loc }: Ast.eq) =
  match eq_desc with
  | EQeq(pat, e) ->
    let e, k = transform_exp e in
    let eq = { eq_desc= EQeq(pat, e); eq_write; eq_loc } in
    k eq
  | EQlocal(v_list, eq) ->
    (* assuming for now that variables with default don't need app isolation *)
    let v_list = transform_vardec_list v_list in
    let eq = transform_eq eq in
    { eq_desc= EQlocal(v_list, eq); eq_write; eq_loc }
  | EQand(and_eq_list) ->
    let and_eq_list = List.map ~f:transform_eq and_eq_list in
    { eq_desc= EQand(and_eq_list); eq_write; eq_loc }
  | EQempty -> { eq_desc= EQempty; eq_write; eq_loc }
  | _ -> assert false

let transform_kind (kind: Ast.kind) =
  match kind with
  | Efun -> Efun
  | Enode -> Enode

let transform_funexp ({ f_kind; f_atomic; f_args; f_res; f_body; f_loc }: Ast.funexp) =
  let f_kind = transform_kind f_kind in
  let f_args = transform_vardec_list f_args in
  let f_res = transform_vardec_list f_res in
  let f_body = transform_eq f_body in
  { f_kind; f_atomic; f_args; f_res; f_body; f_loc }

let transform_program (i_list: Ast.program) =
  let f ({ desc; loc }: Ast.implementation) =
    match desc with
    | Eletfundecl(f, fd) ->
      let fd = transform_funexp fd in
      with_loc (Eletfundecl(f, fd)) loc
    | _ -> assert false
  in
  List.map ~f i_list
