(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Module Builder
    A module functor and respective types to suuport easy
    data aggregation into tables.

*)

type coltype = [ `Int | `Float | `String ] Table.Row.t
(** Specifies name and data type of columns *)

(** Definitions of the underlying data structure, e.g., data columns and their types *)
module type D = sig
  type rowdata
  (** Type of a single data row *)

  val convert : rowdata -> 'a Table.Row.t
end

(** Module signature of a data builder *)
module type S = sig
  type t

  type row_t

  val create : string -> coltype -> t

  val add : row_t -> t -> t

  val to_table : t -> Table.t
end

(** Functor to generate a data table builder *)
module Make (Data : D) : S with type row_t := Data.rowdata = struct
  type t = {
    name : string;
    colspec : [ `Int | `Float | `String ] Table.Row.t;
    rows : Series.data_type Table.Row.t list;
  }

  let create name colspec = { name; colspec; rows = [] }

  let add data builder =
    { builder with rows = Data.convert data :: builder.rows }

  let to_table _builder = failwith "Not implemented"
end
