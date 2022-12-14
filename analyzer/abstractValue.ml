open Ast
open Alarm

let num_node = ref 0
let fresh_node = num_node := !num_node + 1; !num_node

let return_idents =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | k ->
      let ident = Ident.fresh ("$_return_" ^ (Int.to_string (k - 1))) in
      aux (k - 1) (ident :: acc)
  in
  aux 10 []

(* 'a = abstract input element, 'b = abstract output element, 'c = node args, 's internal state *)
type ('a, 'b, 'c, 's) node =
  | ACoFun : ('a -> 'c -> ('b, analyzer_error) Result.t) -> ('a, 'b, 'c, 's) node
  | ACoNode : {
      init: ('a -> (('b * 's), analyzer_error) Result.t);
      step: ('s -> 'a -> 'c -> ('b, analyzer_error) Result.t)
    } -> ('a, 'b, 'c, 's) node
