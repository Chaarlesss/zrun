open Ast
open Alarm

module type S =
sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool

  val is_top: t -> bool

  val is_subset: t -> t -> (bool, analyzer_error) Result.t

  val join: t -> t -> (t, analyzer_error) Result.t

  val meet: t -> t -> (t, analyzer_error) Result.t

  val guard: exp -> t -> (t, analyzer_error) Result.t

  val assign: Ident.t list -> exp -> t -> (t, analyzer_error) Result.t

  val remove: Ident.t list -> t -> (t, analyzer_error) Result.t
  (* when an assert is called *)
  val check: exp -> t -> (t, analyzer_error) Result.t

  val pp: Formatter.t -> t -> unit
end
