(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Module Series
    contains data containers for data series, e.g., integer, float or string.

    It has common types and functions to create and access data series, and
    dedicated (sub-) modules for the concrete types *)

type data_type =
  | DFloat of float
  | DInt of int
  | DStr of string  (** Data type of a single element of a data series *)

type summary = { name : string; data_type : string; length : int }
[@@deriving show]
(** Some key properties of a data series, like its name, type, and length *)

module type S = sig
  type t
  (** Abstract data type for series of data *)

  type dtype
  (** The data type of a single element *)

  val name : t -> string
  (** The name identifier for the data column *)

  val create : name:string -> value:dtype -> int -> t
  (** [create ~name ~value len] creates a data series instance of given
      length [len], name [name], and filled with [value]. *)

  val from_list : name:string -> dtype list -> t
  (** [from_list ~name l] constructs a data series instance from
      given list [l] and assigns [name] to it. *)

  val get : int -> t -> dtype
  (** [get i d] returns the element of [d] at index [i].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val set : int -> dtype -> t -> (int, [> `Invalid_index ]) result
  (** [set i d s] sets the element at index [i] to [d] of series [s].
      If successful, [Ok i] is returned, otherwise, signals [`Invalid_index] as error,
      if [i] is out-of-bounds. *)

  val length : t -> int
  (** [length s] returns the length (= number of entries) of the data series [s]. *)

  val blit : start:int -> t -> t -> (unit, [> `Invalid_length]) result
  (** [blit ~start src dst] copies the content of series [src] to [dst],
      starting at position [start]. If [start] or start + length of src
      are out of bound of [dst], [`Invalid_length] eror is returned. *)
end

module Floats : S with type dtype := float
(** Module Floats for series of float data *)

module Ints : S with type dtype := int
(** Module Ints for series of integer data *)

module Strings : S with type dtype := string
(** Module Strings for series of string data *)

type _ t =
  | SFloat : Floats.t -> float t
  | SInt : Ints.t -> int t
  | SStr : Strings.t -> string t  (** Type for a series of data *)

val name : 'a t -> string
(** The name identifier of a data series *)

val length : 'a t -> int
(** [length s] returns the length of [s]. *)

val get : int -> 'a t -> 'a
(** [get i s] returns the element of [s] at index [i].
    Throws out-of-bounds exception, if [i] is out-of-bounds. *)

val set : int -> 'a -> 'a t -> (int, [> `Invalid_index ]) result
(** [set i d s] sets the element of [d] at index [i] to the value of [d].
    If successful, [Ok i] is returned, otherwise, signals [`Invalid_index] as error,
    if [i] is out-of-bounds. *)

val summary : 'a t -> summary
(** [summary d] returns a short summary of the data series [s]
    with its data type, and some infos on the values, like length, min/max etc. *)

val append : 'a t -> 'a t -> ('a t, [>`Invalid_length]) result
(** [append s1 s2] returns a new data series with [s2] appended to [s1].
      The name identifier is taken from s1. *)
