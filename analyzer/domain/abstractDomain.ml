open AbstractSyntax
open Alarm

module type S =
sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool

  val is_top: t -> bool

  val is_subset: t -> t -> (bool, analyzer_alarm) Result.t

  val join: t -> t -> (t, analyzer_alarm) Result.t

  val meet: t -> t -> (t, analyzer_alarm) Result.t

  (* add a generic widening context? *)
  val widen: t -> t -> (t, analyzer_alarm) Result.t

  val guard: exp -> t -> (t, analyzer_alarm) Result.t

  val assign: Ident.t -> exp -> t -> (t, analyzer_alarm) Result.t

  val remove: Ident.t -> t -> (t, analyzer_alarm) Result.t

  (* when an assert is called *)
  val check: exp -> t -> (t, analyzer_alarm) Result.t

  val pp: Formatter.t -> t -> unit
end
