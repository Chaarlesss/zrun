open Ast

module type DOMAIN =
sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool

  val is_subset: t -> t -> bool

  val join: t -> t -> t

  val meet: t -> t -> t

  val guard: exp -> t -> t

  val assign: pateq -> exp -> t -> t

  (* when an assert is called *)
  val check: exp -> t -> t
end
