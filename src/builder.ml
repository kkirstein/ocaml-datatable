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

  val colspec : [> `Int | `Float | `String ] Table.Row.t

  val convert : record -> Series.data_type Table.Row.t
end

(** Module signature of a data builder *)
module type S = sig
  type t

  type row_t

  val create : string -> t

  val add : row_t -> t -> t

  val to_table :
    t ->
    ( Table.t,
      [> `Invalid_column | `Invalid_datatype | `Invalid_index | `Invalid_length ]
    )
    result
end

(** Functor to generate a data table builder *)
module Make (Data : D) : S with type row_t := Data.record = struct
  type t = {
    name : string;
    (* colspec : [ `Int | `Float | `String ] Table.Row.t; *)
    rows : Series.data_type Table.Row.t list;
  }

  let create name = { name; rows = [] }

  let add data builder =
    { builder with rows = Data.convert data :: builder.rows }

  let create_cols builder dt =
    let open Series in
    let cols = Table.Row.bindings Data.colspec in
    let len = List.length builder.rows in
    let rec loop cols res =
      match res with
      | Ok dt -> (
          match cols with
          | (n, d) :: cs -> (
              match d with
              | `Float ->
                  loop cs
                    (Table.add_col
                       (SFloat (Floats.create ~name:n ~value:0.0 len))
                       dt)
              | `Int ->
                  loop cs
                    (Table.add_col (SInt (Ints.create ~name:n ~value:0 len)) dt)
              | `String ->
                  loop cs
                    (Table.add_col
                       (SStr (Strings.create ~name:n ~value:"" len))
                       dt) )
          | [] -> res )
      | Error _ as e -> e
    in
    loop cols (Ok dt)

  let fill_cols builder dt =
    match
      List.fold_left
        (fun res r ->
          match res with
          | Ok n -> Table.set_row r (n + 1) dt
          | Error _ as e -> e)
        (Ok (-1)) builder.rows
    with
    | Ok _ -> Ok dt
    | Error _ as e -> e

  let to_table builder =
    let ( >>= ) = Result.bind in
    Table.empty builder.name |> create_cols builder >>= fill_cols builder
end
