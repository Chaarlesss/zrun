(* *********************************************************************)
(*                                                                     *)
(*                        The ZRun Interpreter                         *)
(*                                                                     *)
(*                             Marc Pouzet                             *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique. All rights reserved. This file is distributed under   *)
(*  the terms of the INRIA Non-Commercial License Agreement (see the   *)
(*  LICENSE file).                                                     *)
(*                                                                     *)
(* *********************************************************************)

type t =
  | Name : string -> t
  | Modname : qualident -> t

and qualident = { qual : string; id: string }

let compare = compare

let qualidname { qual = m; id = id } = m ^ "." ^ id

let modname = function
  | Name(n) -> n
  | Modname(qualid) -> qualidname qualid

let source = function
  | Name(n) -> n
  | Modname(qualid) -> qualid.id

let fprint_t ff id = Format.fprintf ff "%s" (modname id)

type t_alias = t

module M = struct
  module T = struct
    type t = t_alias

    let compare = compare
    let fprint ff id = Format.fprintf ff "%s" (modname id)

    let sexp_of_t id =
      Sexplib0.Sexp.message "" []
  end
  include T
  include Base.Comparable.Make(T)
end
