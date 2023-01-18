open Ibase.Monad.Result


module Interval = struct
  type bound = PInf | MInf | Int of int
  type t = Interval of bound * bound | Empty
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

  let bottom = Empty
  let top = Interval (PInf, MInf)
  let interval a b = Interval (Int a, Int b)
  let const n = interval n n

  let eq a b =
    match (a, b) with
    | MInf, MInf -> true
    | PInf, PInf -> true
    | x, PInf -> false
    | PInf, x -> false
    | x, MInf -> false
    | MInf, x -> false
    | Int x, Int y -> x = y

  let eqt i1 i2 =
    match (i1, i2) with
    | Empty, Empty -> true
    | Interval (a, b), Interval (c, d) -> eq a c && eq b d
    | _, _ -> false

  let is_bottom i = eqt i bottom

  let leq a b =
    match (a, b) with
    | _, PInf -> true
    | MInf, _ -> true
    | Int x, Int y -> x <= y
    | _, _ -> false

  let geq a b =
    match (a, b) with
    | MInf, _ -> false
    | PInf, _ -> true
    | Int x, Int y -> x >= y
    | _ -> false

  let lt a b = leq a b && not (eq a b)
  let gt a b = geq a b && not (eq a b)

  let is_subset i1 i2 =
    match (i1, i2) with
    | Empty, _ -> return true
    | _, Empty -> return false
    | _, Interval (MInf, PInf) -> return true
    | Interval (MInf, PInf), _ -> return false
    | Interval (a, b), Interval (c, d) -> return (leq c a && leq b d)

  let min a b = if leq a b then a else b
  let max a b = if leq a b then b else a

  let meet i1 i2 =
    match (i1, i2) with
    | Empty, _ | _, Empty -> return Empty
    | Interval (MInf, PInf), x -> return x
    | Interval (a, b), Interval (c, d) ->
        if lt b c || lt d a then return Empty
        else
          let* r = is_subset i1 i2 in
          if r then return i1
          else
            let* r = is_subset i2 i1 in
            if r then return i2
            else if leq a c && leq c b then return (Interval (c, b))
            else return (Interval (c, d))

  let join i1 i2 =
    match (i1, i2) with
    | Empty, x | x, Empty -> return x
    | Interval (a, b), Interval (c, d) -> return (Interval (min a c, max b d))

  let pp_bound = function
    | MInf -> "-oo"
    | PInf -> "+oo"
    | Int x -> string_of_int x

  let pp = function
    | Empty -> "BOT"
    | Interval (x, y) -> "(" ^ pp_bound x ^ " , (" ^ pp_bound y ^ ")"

  let opp_bound b =
    match b with MInf -> PInf | PInf -> MInf | Int a -> Int (-a)

  let opp i =
    match i with
    | Empty -> Empty
    | Interval (a, b) -> Interval (opp_bound b, opp_bound a)

  let plus_bound b1 b2 =
    match (b1, b2) with
    | MInf, Int a -> MInf
    | Int a, MInf -> MInf
    | PInf, Int a -> PInf
    | Int a, PInf -> PInf
    | Int a, Int b -> Int (a + b)
    | _ -> failwith "Not addable bound"

  let minus_bound b1 b2 =
    match (b1, b2) with
    | MInf, Int a -> MInf
    | MInf, PInf -> MInf
    | Int a, MInf -> PInf
    | PInf, Int a -> PInf
    | Int a, PInf -> MInf
    | Int a, Int b -> Int (a - b)
    | _ -> failwith "Not substituable  bound"

  let time_bound b1 b2 =
    match (b1, b2) with
    | Int a, MInf | MInf, Int a -> if a > 0 then MInf else PInf
    | Int a, PInf | PInf, Int a -> if a > 0 then PInf else MInf
    | Int a, Int b -> Int (a * b)
    | PInf, PInf -> PInf
    | MInf, MInf -> PInf
    | _ -> failwith "Not timable  bound"

  let div_bound b1 b2 =
    match (b1, b2) with
    | PInf, Int 0 -> PInf
    | Int a, Int 0 when a > 0 -> PInf
    | Int a, Int 0 when a < 0 -> MInf
    | Int 0, Int 0 -> Int 0
    | Int x, PInf -> Int 0
    | Int x, MInf -> Int 0
    | Int a, Int b -> Int (a / b)
    | _ -> failwith "Not substituable  bound"

  let plus i1 i2 =
    return
      (match (i1, i2) with
      | Empty, x | x, Empty -> Empty
      | Interval (a, b), Interval (c, d) ->
          let b1 = plus_bound a c in
          let b2 = plus_bound b d in
          let b1, b2 = (min b1 b2, max b1 b2) in
          Interval (b1, b2))

  let minus i1 i2 =
    return
      (match (i1, i2) with
      | Empty, x -> Empty
      | x, Empty -> Empty
      | Interval (a, b), Interval (c, d) ->
          let b1 = minus_bound a d in
          let b2 = minus_bound b c in
          let b1, b2 = (min b1 b2, max b1 b2) in
          Interval (b1, b2))

  let time i1 i2 =
    return
      (match (i1, i2) with
      | x, Empty | Empty, x -> Empty
      | Interval (a, b), Interval (c, d) ->
          Interval
            ( min (time_bound a c)
                (min (time_bound a d) (min (time_bound b c) (time_bound b d))),
              max (time_bound a c)
                (max (time_bound a d) (max (time_bound b c) (time_bound b d)))
            ))

  let rec div i1 i2 =
    match (i1, i2) with
    | _, Empty | Empty, _ -> return Empty
    | Interval (_, _), Interval (Int 0, Int 0) -> return Empty
    | Interval (a, b), Interval (c, d) when leq (Int 0) c ->
        return
          (Interval
             ( min (div_bound a c)
                 (min (div_bound a d) (min (div_bound b c) (div_bound b d))),
               max (div_bound a c)
                 (max (div_bound a d) (max (div_bound b c) (div_bound b d))) ))
    | Interval (a, b), Interval (c, d) when leq d (Int 0) ->
        div
          (Interval (opp_bound b, opp_bound a))
          (Interval (opp_bound d, opp_bound c))
    | Interval (a, b), Interval (c, d) ->
        let* r1 = div (Interval (a, b)) (Interval (c, Int 0)) in
        let* r2 = div (Interval (a, b)) (Interval (Int 0, d)) in
        join r1 r2

  let pred b = minus_bound b (Int 1)
  let succ b = plus_bound b (Int 1)

  let filter_leq i1 i2 =
    return
      (match (i1, i2) with
      | Empty, _ | _, Empty -> (i1, i2)
      | Interval (a, b), Interval (c, d) ->
          (Interval (a, min b d), Interval (max a c, d)))

  let filter_geq i1 i2 =
    return
      (match (i1, i2) with
      | Empty, _ | _, Empty -> (i1, i2)
      | Interval (a, b), Interval (c, d) ->
          (Interval (max a c, b), Interval (c, min b d)))

  let filter_lt i1 i2 =
    return
      (match (i1, i2) with
      | Empty, _ | _, Empty -> (i1, i2)
      | Interval (a, b), Interval (c, d) ->
          (Interval (a, min b (pred d)), Interval (max (succ a) c, d)))

  let filter_gt i1 i2 =
    return
      (match (i1, i2) with
      | Empty, _ | _, Empty -> (i1, i2)
      | Interval (a, b), Interval (c, d) ->
          (Interval (max a (succ c), b), Interval (c, min (pred b) d)))

  let filter_eq i1 i2 =
    let* r = meet i1 i2 in
    match r with
    | Empty -> return (Empty, Empty)
    | Interval (a, b) ->
        let i = Interval (a, b) in
        return (i, i)

  let filter_neq i1 i2 =
    return
      (match (i1, i2) with
      | Empty, _ | _, Empty -> (i1, i2)
      | Interval (a, b), Interval (c, d) -> (
          match (eq a b, eq c d) with
          | true, true when eq a c -> (Empty, Empty)
          | true, false when eq a c -> (Interval (a, b), Interval (succ c, d))
          | true, false when eq b d -> (Interval (a, b), Interval (c, pred d))
          | false, true when eq a c -> (Interval (succ a, b), Interval (c, d))
          | false, true when eq c d -> (Interval (a, pred b), Interval (c, d))
          | _ -> (Interval (a, b), Interval (c, d))))

  let bwd_leq0 i = meet i (Interval (MInf, Int 0))
  let bwd_geq0 i = meet i (Interval (Int 0, PInf))
  let bwd_opp i r = meet i (opp r)

  let bwd_plus i1 i2 r =
    let* r0 = minus r i2 in
    let* r1 = meet i1 r0 in
    let* r0 = minus r i1 in
    let* r2 = meet i2 r0 in
    return (r1, r2)

  let bwd_minus i1 i2 r =
    let* r0 = plus r i2 in
    let* r1 = meet i1 r0 in
    let* r0 = minus i1 r in
    let* r2 = meet i2 r0 in
    return (r1, r2)

  let bwd_time i1 i2 r =
    let* r0 = div r i2 in
    let* r1 = meet i1 r0 in
    let* r0 = div r i1 in
    let* r2 = meet i2 r0 in
    return (r1, r2)

  let bwd_div i1 i2 r =
    let* s = plus r (interval (-1) 1) in
    let* r0 = time s i2 in
    let* r1 = meet i1 r0 in
    let* rd = div i1 s in
    let* r0 = join rd (interval 0 0) in
    let* r2 = meet i2 r0 in
    return (r1, r2)

  let contain_n n i = is_subset (const n) i
  let contain_0 = contain_n 0
  let contain_1 = contain_n 1

  let bwd_cond_gen if_one if_z i1 i2 r =
    let* b1 = contain_0 r in
    let* b2 = contain_1 r in
    match (b1, b2) with
    | true, true -> return (i1, i2)
    | true, false -> if_z i1 i2
    | false, true -> if_one i1 i2
    | false, false -> return (Empty, Empty)

  let bwd_eq = bwd_cond_gen filter_eq filter_neq
  let bwd_neq = bwd_cond_gen filter_neq filter_eq
  let bwd_lt = bwd_cond_gen filter_lt filter_geq
  let bwd_gt = bwd_cond_gen filter_gt filter_leq
  let bwd_leq = bwd_cond_gen filter_leq filter_gt
  let bwd_geq = bwd_cond_gen filter_geq filter_lt
  let bwd_fby i1 i2 r = return (i1, i2)
  let bwd_pre i r =  return i

  let guard i1 op i2 =
    match op with
    | LEQ ->  (filter_leq i1 i2)
    | LT ->   (filter_lt i1 i2)
    | EQ ->   (filter_eq i1 i2)
    | GEQ ->  (filter_geq i1 i2)
    | GT ->   (filter_gt i1 i2)
    | NEQ ->  (filter_neq i1 i2)

  let rec bwd_unary op i r =
    match op with
    | ArithU op -> bwd_arith_unary op i r
    | SyncU op -> bwd_sync_unary op i r

  and bwd_arith_unary op i r = match op with OPP -> bwd_opp i r
  and bwd_sync_unary op i r = match op with Pre -> (bwd_pre i r)

  let rec bwd_binary op i1 i2 r =
    match op with
    | ArithB op -> bwd_arith_binary op i1 i2 r
    | Comp op -> bwd_comp op i1 i2 r
    | SyncB op -> bwd_sync_binary op i1 i2 r

  and bwd_arith_binary op i1 i2 r =
    match op with
    | PLUS -> bwd_plus i1 i2 r
    | DIV -> bwd_div i1 i2 r
    | TIME -> bwd_time i1 i2 r
    | MINUS -> bwd_minus i1 i2 r

  and bwd_comp op i1 i2 r =
    match op with
    | LEQ -> bwd_leq i1 i2 r
    | EQ -> bwd_eq i1 i2 r
    | NEQ -> bwd_neq i1 i2 r
    | GEQ -> bwd_geq i1 i2 r
    | LT -> bwd_lt i1 i2 r
    | GT -> bwd_gt i1 i2 r

  and bwd_sync_binary op i1 i2 r = match op with Fby -> (bwd_fby i1 i2 r)

 let widen i1 i2 = 
    return 
    (match i1 i2 with 
    |Empty, _ | _,Empty -> Empty
    | Interval(a1,b1),Interval(a2,b2) -> let a = (if lt a2 a1 then MInf else a1) in
                                         let b = (if lt b2 b1 then PInf else b1) in
                                         Interval(a,b))
                                              
    
  let well_formed = function
    | Interval (a, b) ->
        if not (leq a b) then
          failwith "Lower bound of the interval is not lower"
        else ()
    | _ -> ()
end
(* Non generic testing *)