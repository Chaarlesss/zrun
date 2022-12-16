open Ast
open Alarm

type unop
type binop
type predicate

module type S =
sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> (bool, analyzer_alarm) Result.t

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
