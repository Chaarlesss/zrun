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
  { num : int;        (* a unique index *)
    source : string;  (* the original name in the source *)
  }

let compare id1 id2 = compare id1.num id2.num
let name id = id.source ^ "_" ^ (string_of_int id.num)
let source id = id.source

let num = ref 0
let fresh s = num := !num + 1; { num = !num; source = s }

let fprint_t ff id = Format.fprintf ff "%s" (name id)

type t_alias = t

module M = struct
  module T = struct
    type t = t_alias
    let compare = compare
    let fprint = fprint_t

    let sexp_of_t id =
      Sexplib0.Sexp.message id.source []
  end
  include T
  include Base.Comparable.Make(T)
end

module Env =
struct
  include (Map.Make(M))

  (* let append env0 env = fold add env0 env *)
  let append env0 env =
    fold (fun x v acc -> update x (function _ -> Some(v)) acc)
      env0 env

  let fprint_t fprint_v ff s =
    Format.fprintf ff "@[<hov 2>{@ ";
    iter (fun k v -> Format.fprintf ff "@[%a: %a@]@ " M.fprint k fprint_v v) s;
    Format.fprintf ff "}@]"
end
