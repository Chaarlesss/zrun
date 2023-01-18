open Ast
open Alarm

type unop
type binop
type predicate
(*
module type S =
sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool 

  val is_top: t -> (bool, analyzer_alarm) Result.t

  val is_subset: t -> t -> (bool, analyzer_alarm) Result.t

  val join: t -> t -> (t, analyzer_alarm) Result.t

  val meet: t -> t -> (t, analyzer_alarm) Result.t

  val constant: int -> (t, analyzer_alarm) Result.t

  val unop: unop -> t -> (t, analyzer_alarm) Result.t

  val back_unop: unop -> t -> (t, analyzer_alarm) Result.t

  val binop: binop -> t -> t -> (t, analyzer_alarm) Result.t

  val back_binop: binop -> t -> t -> t -> (t * t, analyzer_alarm) Result.t

  val predicate: predicate -> t -> t -> (t * t, analyzer_alarm) Result.t

  val pp: Formatter.t -> t -> unit
end
*)
module type S = sig
  type t
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

  val bottom : t
  val top : t
  val is_bottom : t -> bool
  val is_subset : t -> t -> (bool, analyzer_alarm) Result.t
  val join : t -> t -> (t, analyzer_alarm) Result.t
  val meet : t -> t -> (t, analyzer_alarm) Result.t
  val guard : t -> compare_op -> t -> (t * t, analyzer_alarm) Result.t
  val const : int -> t
  val plus : t -> t -> t
  val minus : t -> t -> t
  val div : t -> t -> t
  val time : t -> t -> t
  val opp : t -> t
  val pp : t -> string
  val eqt : t -> t -> bool
  val bwd_binary : op_binary -> t -> t -> t -> t * t
  val bwd_unary : op_unary -> t -> t -> t
  (* when an assert is called *)
end
