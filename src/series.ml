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

  val create : name:string -> value:dtype -> int -> t
  val from_list : name:string -> dtype list -> t
  val get : int -> t -> dtype
  val set : int -> dtype -> t -> (int, [> `Invalid_index ]) result
  val length : t -> int
  val blit : start:int -> t -> t -> (unit, [> `Invalid_length ]) result
end

module Numeric = struct
  module type Num = sig
    type dtype
    type dtype_elt

    val dtype_kind : (dtype, dtype_elt) kind
  end

  module Make (N : Num) : S with type dtype := N.dtype = struct
    type t = { name : string; data : (N.dtype, N.dtype_elt, c_layout) Array1.t }

    let name s = s.name

    let create ~name ~value len =
      let ary = Array1.create N.dtype_kind c_layout len in
      Array1.fill ary value;
      { name; data = ary }

    let from_list ~name l =
      { name; data = Array1.of_array N.dtype_kind c_layout (Array.of_list l) }

    let get idx s = Array1.get s.data idx

    let set idx d s =
      try
        Array1.set s.data idx d;
        Ok idx
      with Invalid_argument _ -> Error `Invalid_index

    let length d = Array1.dim d.data

    let blit ~start src dst =
      let src_len = Array1.dim src.data in
      if Array1.dim dst.data < start + src_len then Error `Invalid_length
      else
        let dst_slice = Array1.sub dst.data start src_len in
        Array1.blit src.data dst_slice;
        Ok ()
  end
end

module Generic = struct
  module type Element = sig
    type dtype
  end

  module Make (E : Element) : S with type dtype := E.dtype = struct
    type t = { name : string; data : E.dtype array }

    let name s = s.name
    let create ~name ~value len = { name; data = Array.make len value }
    let from_list ~name l = { name; data = Array.of_list l }
    let get idx s = s.data.(idx)

    let set idx d s =
      try
        s.data.(idx) <- d;
        Ok idx
      with Invalid_argument _ -> Error `Invalid_index

    let length d = Array.length d.data

    let blit ~start src dst =
      let src_len = Array.length src.data in
      if Array.length dst.data < start + src_len then Error `Invalid_length
      else (
        Array.blit src.data 0 dst.data start src_len;
        Ok ())
  end
end

(* Float series *)
module Floats = Numeric.Make (struct
  type dtype = float
  type dtype_elt = float64_elt

  let dtype_kind = float64
end)

(* Int series *)
module Ints = Numeric.Make (struct
  type dtype = int
  type dtype_elt = int_elt

  let dtype_kind = int
end)

(* String series *)
module Strings = Generic.Make (struct
  type dtype = string
end)

(* basic module type for data series *)
type _ t =
  | SFloat : Floats.t -> float t
  | SInt : Ints.t -> int t
  | SStr : Strings.t -> string t

let name : type a. a t -> string = function
  | SFloat s -> Floats.name s
  | SInt s -> Ints.name s
  | SStr s -> Strings.name s

let length : type a. a t -> int = function
  | SFloat s -> Floats.length s
  | SInt s -> Ints.length s
  | SStr s -> Strings.length s

let get : type a. int -> a t -> a =
 fun idx s ->
  match s with
  | SFloat s -> Floats.get idx s
  | SInt s -> Ints.get idx s
  | SStr s -> Strings.get idx s

let set : type a. int -> a -> a t -> (int, [> `Invalid_index ]) result =
 fun idx d s ->
  match s with
  | SFloat s -> Floats.set idx d s
  | SInt s -> Ints.set idx d s
  | SStr s -> Strings.set idx d s

type summary = { name : string; data_type : string; length : int }
[@@deriving show]

let summary : type a. a t -> summary = function
  | SFloat s ->
      { name = Floats.name s; data_type = "float"; length = Floats.length s }
  | SInt s -> { name = Ints.name s; data_type = "int"; length = Ints.length s }
  | SStr s ->
      { name = Strings.name s; data_type = "string"; length = Strings.length s }

let append : type a. a t -> a t -> (a t, [> `Invalid_length ]) result =
 fun s1 s2 ->
  let ( >>= ) = Result.bind in
  match (s1, s2) with
  | SFloat s1, SFloat s2 -> (
      let open Floats in
      let s = create ~name:(name s1) ~value:0.0 (length s1 + length s2) in
      match blit ~start:0 s1 s >>= fun () -> blit ~start:(length s1) s2 s with
      | Ok () -> Ok (SFloat s)
      | Error e -> Error e
      (* NOTE: This shouldn't happen, as we control all indices here, probably thowing an exception here *)
      )
  | SInt s1, SInt s2 -> (
      let open Ints in
      let s = create ~name:(name s1) ~value:0 (length s1 + length s2) in
      match blit ~start:0 s1 s >>= fun () -> blit ~start:(length s1) s2 s with
      | Ok () -> Ok (SInt s)
      | Error e -> Error e)
  | SStr s1, SStr s2 -> (
      let open Strings in
      let s = create ~name:(name s1) ~value:"" (length s1 + length s2) in
      match blit ~start:0 s1 s >>= fun () -> blit ~start:(length s1) s2 s with
      | Ok () -> Ok (SStr s)
      | Error e -> Error e)
