(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

module Row = Map.Make (String)

type column = Col : _ Series.t -> column

type t = { name : string; columns : column list }

type row = Series.data_type Row.t

type summary = { name : string; num_rows : int; column_names : string list }

let empty name = { name; columns = [] }

let add_col : type a. a Series.t -> t -> t =
 fun s dt ->
  let col = Col s in
  { dt with columns = col :: dt.columns }

let summary dt =
  match dt.columns with
  | [] -> { name = dt.name; num_rows = 0; column_names = [] }
  | c :: _ as cs ->
      let len =
        let (Col s) = c in
        Series.length s
      in
      let colnames =
        List.map
          (fun x ->
            let (Col s) = x in
            Series.name s)
          cs
      in
      { name = dt.name; num_rows = len; column_names = colnames }

let get_col name dt =
  match
    List.filter
      (fun c ->
        let (Col s) = c in
        Series.name s = name)
      dt.columns
  with
  | [] -> None
  | cs -> Some (List.hd cs)

(*
let create name data = { name; columns = data }
*)

(*
let get_row ?names idx dt =
  let cols =
    match names with
    | None ->
        List.map
          (fun x ->
            let (Col s) = x in
            Series.name s)
          dt.columns
    | Some n -> n
  in
  let row =
    match cols with
    | [] -> Row.empty
    | _ -> (
        try
          List.fold_left
            (fun acc x ->
              let (Col s) = x in
              let cname = Series.name s in
              if List.mem cname cols then
                Row.add cname (Series.DFloat (Series.get idx s)) acc
              else acc)
            Row.empty dt.columns
        with _ -> Row.empty )
  in
  if Row.is_empty row then None else Some row
*)