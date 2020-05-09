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

  type dtype
  (** The data type of a single element *)

  val name : t -> string
  (** The name identifier for the data column *)

  val from_list : name:string -> dtype list -> t
  (** [from_list ~name l] constructs a data series instance from
      given list [l] and assigns [name] to it. *)

  val get : int -> t -> dtype
  (** [get i d] returns the element of [d] at index [i].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val set : int -> dtype -> t -> unit
  (** [set i d s] sets the element at index [i] to [d] of series [s].
      Throws out-of-bounds exception, if [i] is out-of-bounds. *)

  val length : t -> int
  (** [length d] returns the length (= number of entries) of the data series [s]. *)
end



module Numeric = struct
  module type Num = sig
    type dtype
    type dtype_elt
    val dtype_kind : (dtype, dtype_elt) kind
  end

  module Make (N : Num) : (S with type dtype := N.dtype) = struct
  type t = { name : string; data : (N.dtype, N.dtype_elt, c_layout) Array1.t }

  let name s = s.name

  let from_list ~name l =
    { name; data = Array1.of_array N.dtype_kind c_layout (Array.of_list l) }

  let get idx s = Array1.get s.data idx

  let set idx d s = Array1.set s.data idx d

  let length d = Array1.dim d.data
  end
end


module Generic = struct
  module type Element = sig
    type dtype
  end

  module Make (E : Element) : (S with type dtype := E.dtype) = struct
  type t = { name : string; data : E.dtype array }

  let name s = s.name

  let from_list ~name l = { name; data = Array.of_list l }

  let get idx s = s.data.(idx)

  let set idx d s = s.data.(idx) <- d

  let length d = Array.length d.data

  end

end


(* Float series *)
module Floats = Numeric.Make(struct
    type dtype = float
    type dtype_elt = float64_elt
    let dtype_kind = float64
  end)

(* Int series *)
module Ints = Numeric.Make(struct
    type dtype = int
    type dtype_elt = int_elt
    let dtype_kind = int
  end)

(* String series *)
module Strings = Generic.Make(struct
    type dtype = string
  end)


(* basic module type for data series *)
(* FIXME: maybe a GADT could help to reduce boilerplate code here *)
type t = SFloat of Floats.t | SInt of Ints.t | SStr of Strings.t

let name = function
  | SFloat s -> Floats.name s
  | SInt s -> Ints.name s
  | SStr s -> Strings.name s

let get idx = function
  | SFloat s -> DFloat (Floats.get idx s)
  | SInt s -> DInt (Ints.get idx s)
  | SStr s -> DStr (Strings.get idx s)

(*let set idx d = function
  | SFloat s -> Floats.set idx d s
  | SInt s -> Ints.set idx d s
  | SStr s -> Strings.set idx d s *)
let set _idx _d _s = failwith "Not implemented"

type summary = { name : string; data_type : string; length : int }

let summary = function
  | SFloat s ->
      { name = Floats.name s; data_type = "float"; length = Floats.length s }
  | SInt s ->
      { name = Ints.name s; data_type = "int"; length = Ints.length s }
  | SStr s ->
      { name = Strings.name s; data_type = "string"; length = Strings.length s }

