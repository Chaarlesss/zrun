open CartesianDomain

open AbstractSyntax
open Ibase.Monad.Result
open Alarm

module Make(First:CartesianDomain.S)(Second:CartesianDomain.S) =
struct

  type t = First.t * Second.t
    type compare_op = LEQ | LT | EQ | GEQ | GT | NEQ
  type arith_op_binary = PLUS | DIV | TIME | MINUS
  type arith_op_unary = OPP
  type sync_op_unary = Pre
  type sync_op_binary = Fby
  type op_unary = ArithU of arith_op_unary | SyncU of sync_op_unary

  type op_binary =
    | Comp of compare_op
    | SyncB of sync_op_binary
    | ArithB of arith_op_binary


  let bottom : t = First.bottom, Second.bottom

  let top : t = First.top, Second.top

  let singleton (x:t) : t = x

  let map_fst (f:(First.t -> First.t)) ((a,b) as x:t) : t =
    let a' = f a in
    if a' == a then x else (a',b)

  let map_snd (f:(Second.t -> Second.t)) ((a,b) as x:t) : t =
    let b' = f b in
    if b == b' then x else (a,b')

  let is_bottom ((a,b):t) =
    First.is_bottom a ||   Second.is_bottom b   

  let is_subset ((a1,b1):t) ((a2,b2):t)  =
    let* r1 = First.is_subset a1 a2 in
    let* r2 = Second.is_subset b1 b2  in 
    return (r1 && r2)
  let apply f1 f2 ((v1,v2) as v) =
    let* r1 = f1 v1 in
    let* r2 = f2 v2 in
    if r1 == v1 && r2 == v2 then return v
    else return (r1,r2)

  let apply2 f1 f2 ((v1,v2) as v) ((w1,w2) as w) =
    let* r1 = f1 v1 w1 in
    let* r2 = f2 v2 w2 in
    if r1 == v1 && r2 == v2 then return v else
    if r1 == w1 && r2 == w2 then return  w
    else  return (r1,r2)

  let join ((v1,v2) as v:t) ((w1,w2) as w:t) =
    if v1 == w1 && v2 == w2 then return  v else
    apply2 First.join Second.join v w

  let meet ((v1,v2) as v:t) ((w1,w2) as w:t) =
    if v1 == w1 && v2 == w2 then return v else
    apply2 First.meet Second.meet v w


  let pp  ((a,b):t) = "("^(First.pp a )^","^(Second.pp b) ^")"
  

end

