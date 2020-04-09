(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

open Bigarray

module type Series = sig
  type t
  (** Abstract data type for series of data *)

  type data_type
  (** The data type of a single element *)

  val name : t -> string
  (** The name identifier for the data column *)

  val from_strings : string list -> string -> t
  (** [from_string strs name] contructs a data series from the
      given list of strings [strs] *)

  val get : int -> t -> data_type
  (** [get i d] returns the element of [d] at index [i].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val set : int -> data_type -> t -> t
  (** [set i d s] sets the element at index [i] to [d] of series [s].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)
end


module type Series_element = sig
  type t
  (** Abstract data type for series of data *)

  type el_t
  (** The data type of a single element *)

  val from_string : string -> t option
  (** [from_string s] converts given string [s] to the
      desired (numerical) type. Returns [None], if convesion fails *)
end


(* Float series *)
module Floats = struct

  type t = {
    name : string;
    data : (float, float32_elt, c_layout) Array1.t
  }
  type data_type = float

end


(* Int series *)
module Ints = struct

  type t = {
    name : string;
    data : (int, int_elt, c_layout) Array1.t
  }
  type data_type =  int

end


(* String series *)
module Strings = struct

  type t = {
    name : string;
    data : string array
  }
  type data_type = string

end


type t =
  | Float of Floats.t
  | Int of Ints.t
  | Str of Strings.t


type data_type =
  | DFloat of float
  | DInt of int
  | DStr of string



