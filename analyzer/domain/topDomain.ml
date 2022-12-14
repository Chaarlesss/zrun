open Ast
open Ibase.Monad.Result
open Alarm

type t = Top

let bottom = Top

let top = Top

let is_bottom (_: t) = true

let is_top (_: t) = true

let is_subset (_: t) (_: t) = return true

let join (_: t) (_: t) = return Top

let meet (_: t) (_: t) = return Top

let guard (_: exp) (_: t) = return Top

let assign (_: Ident.t list) (_: exp) (_: t) = return Top

let remove (_: Ident.t list) (_: t) = return Top

let check (_: exp) (_: t) = return Top

let pp formatter (_: t) =
  Caml.Format.pp_print_string formatter "_|_"
