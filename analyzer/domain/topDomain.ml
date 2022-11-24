open Ast

type t = Top

let bottom = Top

let top = Top

let is_bottom (_: t) = true

let is_subset (_: t) (_: t) = true

let join (_: t) (_: t) = Top

let meet (_: t) (_: t) = Top

let guard (_: exp) (_: t) = Top

let assign (_: pateq) (_: exp) (_: t) = Top

let check (_: exp) (_: t) = Top
