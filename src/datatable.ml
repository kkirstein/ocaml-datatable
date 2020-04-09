(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)


type t = {
  name : string;
  columns : Series.t list
}

let create name data =
  {name; columns = data}

let get_row _i _dt = None

let get_col _names _dt = None

