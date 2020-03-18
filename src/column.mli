(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)


type t
  (** Abstract data type, for a data series, aka column *)

type data_type =
  | Float
  | Int
  | Str
  | Adt

module type Column_type = sig
end

module type Data_type = sig
end


