(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)


type t
(** Abstract data type, for a data series, aka column *)

type data_type =
  | Float of float
  | Int of int
  | Str of string


module type Column_type = sig
  type t
  (** Abstract data type for series of data *)

  type el_t
  (** The data type of a single element *)

  val name : t -> string
  (** The name identifier for the data column *)
end


module type Data_type = sig
  type t
  (** Abstract data type for series of data *)

  type el_t
  (** The data type of a single element *)

  val get : int -> el_t
  (** [get i] returns the element at index [i].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val set : int -> el_t -> t -> t
  (** [set i d s] sets the element at index [i] to [d] of series [s].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

end


