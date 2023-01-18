
open AbstractSyntax
open Ibase.Monad.Result
open Alarm

module Sign = struct
  type t = TOP | BOT | GEQZ | LEQZ | NZ | Z | LTZ | GTZ
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

  let bottom = BOT
  let top = TOP
  let eqt = ( = )
  let is_bottom i = match i with BOT -> true | _ -> false

  let join s1 s2 =
    return
      (match (s1, s2) with
      | x, y when x == y -> x
      | BOT, x | x, BOT -> x
      | LTZ, Z | Z, LTZ | LEQZ, Z | Z, LEQZ | LEQZ, LTZ | LTZ, LEQZ -> LEQZ
      | GTZ, Z | Z, GTZ | GEQZ, Z | Z, GEQZ | GEQZ, LTZ | LTZ, GEQZ -> GEQZ
      | LTZ, GTZ | GTZ, LTZ | GTZ, NZ | NZ, GTZ | LTZ, NZ | NZ, LTZ -> NZ
      | _, _ -> TOP)

  let meet s1 s2 =
    return
      (match (s1, s2) with
      | x, y when x == y -> x
      | TOP, x | x, TOP -> x
      | LEQZ, NZ | NZ, LEQZ | LTZ, LEQZ | LEQZ, LTZ | NZ, LTZ | LTZ, NZ -> LTZ
      | LEQZ, GEQZ | GEQZ, LEQZ | Z, LEQZ | LEQZ, Z | GEQZ, Z | Z, GEQZ -> Z
      | GEQZ, NZ | NZ, GEQZ | GTZ, GEQZ | GEQZ, GTZ | NZ, GTZ | GTZ, NZ -> GTZ
      | _, _ -> BOT)

  let is_subset s1 s2 =
    let* r = meet s1 s2 in
    return (r == s2)

  let pp = function
    | TOP -> "TOP"
    | BOT -> "BOT"
    | GEQZ -> ">=0"
    | LEQZ -> "<=0"
    | NZ -> ">0"
    | Z -> "=0"
    | LTZ -> "<0"
    | GTZ -> ">0"

  let opp = function
    | TOP -> TOP
    | BOT -> BOT
    | GEQZ -> LEQZ
    | LEQZ -> GEQZ
    | NZ -> NZ
    | Z -> Z
    | LTZ -> GTZ
    | GTZ -> GTZ

  let plus s1 s2 =
    return
      (match (s1, s2) with
      | x, y when x == y -> x
      | BOT, _ | _, BOT -> BOT
      | x, Z | Z, x -> x
      | GEQZ, GTZ | GTZ, GEQZ -> GTZ
      | LEQZ, LTZ | LTZ, LEQZ -> LTZ
      | _, _ -> TOP)

  let time s1 s2 =
    return
      (match (s1, s2) with
      | BOT, _ | _, BOT -> BOT
      | GTZ, GTZ | LTZ, LTZ -> GTZ
      | LTZ, GTZ | GTZ, LTZ -> LTZ
      | GEQZ, GEQZ | LEQZ, LEQZ | GTZ, GEQZ | GEQZ, GTZ | LEQZ, LTZ | LTZ, LEQZ
        ->
          GEQZ
      | GEQZ, LEQZ | LEQZ, GEQZ | GEQZ, LTZ | LTZ, GEQZ -> LEQZ
      | Z, _ | _, Z -> Z
      | NZ, NZ -> NZ
      | _, _ -> TOP)

  let minus s1 s2 =
    return
      (match (s1, s2) with
      | BOT, _ | _, BOT -> BOT
      | Z, LTZ -> GTZ
      | Z, GTZ -> LTZ
      | Z, LEQZ -> GEQZ
      | Z, GEQZ -> LEQZ
      | x, Z -> x
      | _, _ -> TOP)

  let div s1 s2 =
    return
      (match (s1, s2) with
      | BOT, _ | _, BOT -> BOT
      | GTZ, GTZ | LTZ, LTZ -> GTZ
      | LTZ, GTZ | GTZ, LTZ -> LTZ
      | NZ, NZ -> NZ
      | _, _ -> TOP)

  let pred s =
    match s with
    | BOT -> BOT
    | TOP -> TOP
    | NZ -> NZ
    | LTZ -> BOT
    | LEQZ -> LTZ
    | Z -> LTZ
    | GEQZ -> Z
    | GTZ -> GEQZ

  let succ s =
    match s with
    | BOT -> BOT
    | TOP -> TOP
    | NZ -> NZ
    | LTZ -> LEQZ
    | LEQZ -> Z
    | Z -> GEQZ
    | GEQZ -> GTZ
    | GTZ -> TOP

  let filter_gen s1 s2 f g =
    match (s1, s2) with
    | _, BOT | BOT, _ -> return (BOT, BOT)
    | s1, s2 ->
        let* r1 = meet (f s1) (g s2) in
        let* r2 = join (f s1) (g s2) in
        return (r1, r2)

  let id x = x
  let filter_leq s1 s2 = filter_gen s1 s2 id id

  let filter_geq s1 s2 =
    let* r = filter_gen s2 s1 id id in
    return (snd r, fst r)

  let filter_lt s1 s2 = filter_gen s1 s2 pred succ

  let filter_gt s1 s2 =
    let* r = filter_gen s2 s1 pred succ in
    return (snd r, fst r)

  let filter_eq s1 s2 =
    let* r = meet s1 s2 in
    match r with BOT -> return (BOT, BOT) | _ -> return (s1, s2)

  let filter_neq s1 s2 =
    return
      (match (s1, s2) with
      | BOT, _ | _, BOT -> (s1, s2)
      | s1, s2 -> if s1 == s2 then (BOT, BOT) else (s1, s2))

  let bwd_opp s r = meet s (opp r)

  let bwd_plus s1 s2 r =
    let* r1 = minus r s2 in
    let* r2 = minus r s1 in
    let* r1 = meet s1 r1 in
    let* r2 = meet s2 r2 in
    return (r1, r2)

  let bwd_minus s1 s2 r =
    let* r1 = plus r s2 in
    let* r2 = minus s1 r in
    let* r1 = meet s1 r1 in
    let* r2 = meet s2 r2 in
    return (r1, r2)

  let bwd_time s1 s2 r =
    let* r1 = div r s2 in
    let* r2 = div r s1 in
    let* r1 = meet s1 r1 in
    let* r2 = meet s2 r2 in
    return (r1, r2)

  let bwd_div s1 s2 r =
    let* r1 = time r s2 in
    let* r2 = div s1 r in
    let* r3 = join r2 Z in
    let* r1 = meet s1 r1 in
    let* r2 = meet s2 r3 in
    return (r1, r2)

  let contain_n n s =
    return
      (match s with
      | TOP -> true
      | BOT -> false
      | GEQZ -> n >= 0
      | LEQZ -> n <= 0
      | NZ -> n <> 0
      | Z -> n = 0
      | LTZ -> n < 0
      | GTZ -> n > 0)

  let contain_0 = contain_n 0
  let contain_1 = contain_n 1

  let bwd_cond_gen if_one if_z i1 i2 r =
    let* b1 = contain_0 r in
    let* b2 = contain_1 r in
    match (b1, b2) with
    | true, true -> return (i1, i2)
    | true, false -> if_z i1 i2
    | false, true -> if_one i1 i2
    | false, false -> return (BOT, BOT)

  let bwd_eq = bwd_cond_gen filter_eq filter_neq
  let bwd_neq = bwd_cond_gen filter_neq filter_eq
  let bwd_lt = bwd_cond_gen filter_lt filter_geq
  let bwd_gt = bwd_cond_gen filter_gt filter_leq
  let bwd_leq = bwd_cond_gen filter_leq filter_gt
  let bwd_geq = bwd_cond_gen filter_geq filter_lt
  let bwd_fby s1 s2 r = return (s1, s2)
  let bwd_pre s r = return  s

  let guard s1 op s2 =
    match op with
    | LEQ -> filter_leq s1 s2
    | LT -> filter_lt s1 s2
    | EQ -> filter_eq s1 s2
    | GEQ -> filter_geq s1 s2
    | GT -> filter_gt s1 s2
    | NEQ -> filter_neq s1 s2

  let rec bwd_unary op s r =
    match op with
    | ArithU op -> bwd_arith_unary op s r
    | SyncU op -> bwd_sync_unary op s r

  and bwd_arith_unary op s r = match op with OPP -> bwd_opp s r
  and bwd_sync_unary op s r = match op with Pre -> bwd_pre s r

  let rec bwd_binary op s1 s2 r =
    match op with
    | ArithB op -> bwd_arith_binary op s1 s2 r
    | Comp op -> bwd_comp op s1 s2 r
    | SyncB op -> bwd_sync_binary op s1 s2 r

  and bwd_arith_binary op s1 s2 r =
    match op with
    | PLUS -> bwd_plus s1 s2 r
    | DIV -> bwd_div s1 s2 r
    | TIME -> bwd_time s1 s2 r
    | MINUS -> bwd_minus s1 s2 r

  and bwd_comp op s1 s2 r =
    match op with
    | LEQ -> bwd_leq s1 s2 r
    | EQ -> bwd_eq s1 s2 r
    | NEQ -> bwd_neq s1 s2 r
    | GEQ -> bwd_geq s1 s2 r
    | LT -> bwd_lt s1 s2 r
    | GT -> bwd_gt s1 s2 r

  and bwd_sync_binary op s1 s2 r = match op with Fby -> bwd_fby s1 s2 r
end
(* Non generic testing *)
