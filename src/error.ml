(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Module Error
    An utility module for easy error handling.

    I supports a common polymorphic variant for unified error handling
    for all modules of this package. *)

let to_string = function
  | `Invalid_index -> "Invalid index"
  | `Invalid_column -> "Invalid column"
  | `Invalid_datatype -> "Invalid data type"
  | `Invalid_length -> "Invalid length"

let result_to_string = function Ok _ -> "Ok" | Error e -> to_string e
