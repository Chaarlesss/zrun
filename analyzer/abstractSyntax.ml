
module S = Ast.S

type 'a localized = 'a Ast.localized

type pateq = pateq_desc localized

and pateq_desc = Ident.t list

type const =
  | Eint: int -> const
  | Ebool: bool -> const

type 'a default =
  | Ewith_nothing: 'a default

type 'exp vardec =
  {
    var_name: Ident.t;
    var_default: 'exp default;
    var_loc: Location.t
  }

type binop =
  | Eadd: binop
  | Efby: binop

type is_rec = bool

type exp = { e_desc: exp_desc; e_loc: Location.t }

and exp_desc =
  | Econst: const -> exp_desc
  | Econstr0: Lident.t -> exp_desc
  | Econstr1: Lident.t * exp list -> exp_desc
  | Elocal: Ident.t -> exp_desc
  | Eglobal: Lident.t -> exp_desc
  | Elast: Ident.t -> exp_desc
  | Ebinop: binop * exp * exp -> exp_desc

and eq =
  {
    eq_desc: eq_desc; (* descriptor *)
    eq_write: S.t; (* set of defined variables *)
    eq_loc: Location.t; (* its location *)
  }

and eq_desc =
  | EQeq: pateq * exp -> eq_desc
  | EQapp: pateq * Lident.t * exp list -> eq_desc
  | EQlocal: exp vardec list * eq -> eq_desc
  | EQand: eq list -> eq_desc
  | EQempty: eq_desc

type kind =
  | Efun: kind
  | Enode: kind

type is_atomic = bool

type funexp =
  {
    f_kind: kind;
    f_atomic: is_atomic;
    f_args: exp vardec list;
    f_res: exp vardec list;
    f_body: eq;
    f_loc: Location.t
  }

type name = String.t

type implementation = implementation_desc localized

and implementation_desc =
  | Eletfundecl: name * funexp -> implementation_desc

type program = implementation list
