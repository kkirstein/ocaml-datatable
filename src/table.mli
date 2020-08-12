(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Datatable library

    {e %%VERION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

type t
(** Abstract data type holding tabular data *)

module Row : Map.S with type key = string

type row = Series.data_type Row.t
(** Data type to hold data of a single row. *)

type summary = { name : string; num_rows : int; column_names : string list }
[@@deriving show]
(** Type for summary of a data table *)

type column =
  | Col : _ Series.t -> column
      (** Type of a single column with different data types *)

val empty : string -> t
(** [empty name] creates an empty datatable with given [name]. *)

val add_col : 'a Series.t -> t -> (t, [> `Invalid_length ]) result
(** [add_col s dt] adds the data series [s] as column to the data table [dt].
    It checks that the length of the new data series matches the data table and returns
    an [`Invalid_length] error otherwise. *)

val length : t -> int
(** [length dt] returns the length (= nmumber of rows) of given data table. *)

val summary : t -> summary
(** [summary dt] gives a short summary of given data table [dt], including
    its name, length (number of rows) and column names. *)

(*
val create : string -> 'a Series.t list -> t
(** [create name data] creates a datatable with given [name] and [data].
    [data] can be left empty. *)
  *)

val get_col : string -> t -> column option
(** [get_col name dt] Returns the column of given name of datatable [dt].
    Returns None if [name] does not exist. If more than one column of
    given name is present, only the first one found is returned. *)

val get_row : ?names:string list -> int -> t -> row option
(** [get_row i dt] Returns the data row of given numeric index [i] of datatable [dt].
    Return None if index is out-of-bounds. *)

val set_row :
  row ->
  int ->
  t ->
  (int, [> `Invalid_column | `Invalid_datatype | `Invalid_index ]) result
(** [set_row r idx dt] Sets the data row of given numeric index [idx] of datatable [dt].
    If successful [Ok idx] is returned.
    Returns respective error, if either index or column anme are invalid. *)

val append : t -> t -> (t, [> `Invalid_column | `Invalid_datatype | `Invalid_length]) result
(** [concat dt1 dt2] appends [dt2] to [dt1].
    If a [dt2] table contains a column not found in the first one,
    an [`Invalid_column] error is returned. *)
