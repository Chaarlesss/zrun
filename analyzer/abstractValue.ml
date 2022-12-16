open Ast
open Alarm

let aux_var_counter = ref 0
let fresh_aux_var () = aux_var_counter := !aux_var_counter + 1; !aux_var_counter

let create_var s =
  let id = fresh_aux_var () in
  Ident.fresh ("$_" ^ s ^ "_$" ^ (Int.to_string id))

let return_idents =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | k ->
      let ident = create_var ("return_" ^ (Int.to_string (k - 1))) in
      aux (k - 1) (ident :: acc)
  in
  aux 10 []

type apvalue =
  | AVident : Ident.t -> apvalue

type avalue = apvalue

type astate =
  | ASempty : astate
  | AStuple : astate list -> astate
  | ASval : avalue -> astate

(* 'a = abstract input element, 'b = abstract output element, 'c = node args, 's internal state *)
type ('a, 'b, 'c, 's, 'e) anode =
  | ACoFun : ('a -> 'c -> ('b, 'e) Result.t) -> ('a, 'b, 'c, 's, 'e) anode
  | ACoNode : {
      init: ('a -> (('b * 's), 'e) Result.t);
      step: ('s -> 'a -> 'c -> ('b, 'e) Result.t)
    } -> ('a, 'b, 'c, 's, 'e) anode

type 'a gvalue =
  | Gfun : ('a, 'a, exp list, astate, analyzer_alarm) anode -> 'a gvalue
