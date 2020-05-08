(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

open Bigarray

(* common types, e.g., for row data *)
type data_type = DFloat of float | DInt of int | DStr of string

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

(*
module type S_el = sig
  type t
  (** Abstract data type for series of data *)

  type el_t
  (** The data type of a single element *)

  val from_string : string -> t option
  (** [from_string s] converts given string [s] to the
      desired (numerical) type. Returns [None], if convesion fails *)

end
*)

(* Float series *)
module Floats : S with type el_t := float = struct
  type t = { name : string; data : (float, float64_elt, c_layout) Array1.t }

  let name s = s.name

  let from_list ~name l =
    { name; data = Array1.of_array float64 c_layout (Array.of_list l) }

  let from_strings ~name _strs =
    let _ = name in
    failwith "Not implemented"

  let get idx s = DFloat (Array1.get s.data idx)

  let set idx d s =
    match d with
    | DFloat d -> Array1.set s.data idx d
    | _ -> failwith "Invalid data type"

  let length d = Array1.dim d.data
end

(* Int series *)
module Ints : S with type el_t := int = struct
  type t = { name : string; data : (int, int_elt, c_layout) Array1.t }

  let name s = s.name

  let from_list ~name l =
    { name; data = Array1.of_array int c_layout (Array.of_list l) }

  let from_strings ~name _strs =
    let _ = name in
    failwith "Not implemented"

  let get idx s = DInt (Array1.get s.data idx)

  let set idx d s =
    match d with
    | DInt d -> Array1.set s.data idx d
    | _ -> failwith "Invalid data type"

  let length d = Array1.dim d.data
end

(* String series *)
module Strings : S with type el_t := string = struct
  type t = { name : string; data : string array }

  let name s = s.name

  let from_list ~name l = { name; data = Array.of_list l }

  let from_strings ~name _strs =
    let _ = name in
    failwith "Not implemented"

  let get idx s = DStr s.data.(idx)

  let set idx d s =
    match d with
    | DStr d -> s.data.(idx) <- d
    | _ -> failwith "Invalid data type"

  let length d = Array.length d.data
end

(* basic module type for data series *)
(* FIXME: maybe a GADT could help to reduce boilerplate code here *)
type t = SFloat of Floats.t | SInt of Ints.t | SStr of Strings.t

let name = function
  | SFloat s -> Floats.name s
  | SInt s -> Ints.name s
  | SStr s -> Strings.name s

let get idx = function
  | SFloat s -> Floats.get idx s
  | SInt s -> Ints.get idx s
  | SStr s -> Strings.get idx s

type summary = { name : string; data_type : string; length : int }

let summary = function
  | SFloat s ->
      { name = Floats.name s; data_type = "float"; length = Floats.length s }
  | SInt s ->
      { name = Ints.name s; data_type = "int"; length = Ints.length s }
  | SStr s ->
      { name = Strings.name s; data_type = "string"; length = Strings.length s }
