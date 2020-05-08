(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Module Series
    contains data containers for data series, e.g., integer, float or string.

    I has common types and functions to create and access data series, and
    dedicated (sub-) modules for the concrete types *)

type data_type =
  | DFloat of float
  | DInt of int
  | DStr of string  (** Data type of a single element of a data series *)

type summary = { name : string; data_type : string; length : int }
(** Some key properties of a data series, like its name, type, and length *)

module type S = sig
  type t
  (** Abstract data type for series of data *)

  type el_t
  (** The data type of a single element *)

  val name : t -> string
  (** The name identifier for the data column *)

  val from_list : name:string -> el_t list -> t
  (** [from_list ~name l] constructs a data series instance from
      given list [l] and assigns [name] to it. *)

  val from_strings : name:string -> string list -> t
  (** [from_string strs name] contructs a data series from the
      given list of strings [strs] *)

  val get : int -> t -> data_type
  (** [get i d] returns the element of [d] at index [i].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val set : int -> data_type -> t -> unit
  (** [set i d s] sets the element at index [i] to [d] of series [s].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val length : t -> int
  (** [length d] returns the length (= number of entries) of the data series [s]. *)
end

module Floats : S with type el_t := float
(** Module Floats for series of float data *)

module Ints : S with type el_t := int
(** Module Ints for series of integer data *)

module Strings : S with type el_t := string
(** Module Strings for series of string data *)

type t =
  | SFloat of Floats.t
  | SInt of Ints.t
  | SStr of Strings.t  (** Type for a series of data *)

val name : t -> string
(** The name identifier of a data series *)

val get : int -> t -> data_type
(** [get i d] returns the element of [d] at index [i].
    Throws out-of-bounds exception, if [i] is out-of-bounds. *)

val summary : t -> summary
(** [summary d] returns a short summary of the data series [s]
    with its data type, and some infos on the values, like length, min/max etc. *)
