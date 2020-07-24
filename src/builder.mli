(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Module Builder
    A module functor and respective types to suuport easy
    data aggregation into tables.

*)

(** Definitions of the underlying data structure, e.g., data columns and their types *)
module type D = sig
  type record
  (** Type of a single data row *)

  val colspec : [> `Int | `Float | `String ] Table.Row.t
  (** defines the names and datatypes of columns of the built table *)

  val convert : record -> Series.data_type Table.Row.t
  (** [convert record] converts given data [record] to a suitable data row type *)
end

(** Module signature of a data builder *)
module type S = sig
  type t
  (** Abstract data type of a table builder *)

  type row_t
  (** Abstract data type of a single data row *)

  val create : string -> t
  (** [create name] creates a builder instance with given [name] *)

  val add : row_t -> t -> t
  (** [add record builder] adds a record to the given table builder
      and returns its instance *)

  val to_table :
    t ->
    ( Table.t,
      [> `Invalid_column | `Invalid_datatype | `Invalid_index | `Invalid_length ]
    )
    result
    (** [to_table builder] converts the buider instance to a data table. *)
end

module Make : functor (Data : D) -> S with type row_t := Data.record
(** Module functor to create a table builder with given column specification
    and record converter. *)
