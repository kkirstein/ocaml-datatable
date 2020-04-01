(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(** Datatable library

    {e %%VERION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)


type t
(** Abstract data type holding tabular data *)


val get_col : string -> Series.t option
(** [get_col name] Returns the column of given name.
    Returns None if [name] does not exist. *)


val get_row : int -> Series.data_type list option
(** [get_row i] Returns the data row of given numeric index [i].
 Return None if index is out-of-bounds. *)


