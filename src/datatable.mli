(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Datatable library

    {e %%VERION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)


type t
(** Abstract data type holding tabular data *)


val get_col : string -> t -> Series.t option
(** [get_col name dt] Returns the column of given name of datatable [dt].
    Returns None if [name] does not exist. *)


val get_row : int -> t -> Series.data_type list option
(** [get_row i dt] Returns the data row of given numeric index [i] of datatable [dt].
    Return None if index is out-of-bounds. *)


val create : string -> Series.t list -> t
(** [create name data] creates a datatable with given [name] and [data].
    [data] can be left empty. *)
