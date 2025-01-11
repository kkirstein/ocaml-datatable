(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2025 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Datatable library

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%%}homepage}} *)

val read_csv :
  builder:(module Builder.S with type row_t = string list) ->
  string ->
  ( Table.t,
    [> `Invalid_column | `Invalid_datatype | `Invalid_index | `Invalid_length ]
  )
  result
(** [read_csv ~col_spec filepath] reads the CSV file given by path [filepath]
    and returns a data table of its data. The column types must be specified by
    [col_spec]. in case of data type mismatch or mismatch of column count, a
    respective error is retured. *)

val write_csv : string -> Table.t -> (unit, [> `Io_error ]) result
(** [write_csv filepath dt] writes the data on data table [dt] to a CSV file
    given by path [filepath]. In case of success [Ok ()] is returned, a
    respective error otherwise. *)
