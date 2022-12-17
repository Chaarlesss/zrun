open AbstractSyntax
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

let widen (_: t) (_: t) = return Top

let guard (_: exp) (_: t) = return Top

let assign (_: Ident.t) (_: exp) (_: t) = return Top

let remove (_: Ident.t) (_: t) = return Top

let check (_: exp) (_: t) = return Top

let pp ff (_: t) =
  Format.pp_print_string ff "T"
