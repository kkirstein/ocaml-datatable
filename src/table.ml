(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

module Row = Map.Make (String)

type column = Col : _ Series.t -> column

type t = { name : string; length : int option; columns : column list }

(*
type row_data = [ `Float of float | `Int of int | `String of string ]
*)
type row = Series.data_type Row.t

type summary = { name : string; num_rows : int; column_names : string list }
[@@deriving show]

let empty name = { name; length = None; columns = [] }

let add_col : type a. a Series.t -> t -> (t, [> `Invalid_length ]) result =
 fun s dt ->
  match dt.length with
  | None ->
      let col = Col s in
      Ok
        { dt with length = Some (Series.length s); columns = col :: dt.columns }
  | Some l ->
      if Series.length s = l then
        let col = Col s in
        Ok { dt with columns = col :: dt.columns }
      else Error `Invalid_length

let length dt = match dt.length with None -> 0 | Some i -> i

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
        |> List.sort compare
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
                match s with
                | Series.SFloat _ ->
                    Row.add cname (Series.DFloat (Series.get idx s)) acc
                | Series.SInt _ ->
                    Row.add cname (Series.DInt (Series.get idx s)) acc
                | Series.SStr _ ->
                    Row.add cname (Series.DStr (Series.get idx s)) acc
              else acc)
            Row.empty dt.columns
        with _ -> Row.empty )
  in
  if Row.is_empty row then None else Some row

let set_row r idx dt =
  let cols = Row.bindings r in
  List.fold_left (* TODO: Propagate error thru iteration *)
    (fun res (n, d) ->
      match res with
      | Ok _ -> (
          match get_col n dt with
          | Some (Col s) -> (
              match (d, s) with
              | Series.DFloat df, Series.SFloat s -> Series.Floats.set idx df s
              | Series.DInt di, Series.SInt s -> Series.Ints.set idx di s
              | Series.DStr ds, Series.SStr s -> Series.Strings.set idx ds s
              | _ -> Error `Invalid_datatype )
          | None -> Error `Invalid_column )
      | _ -> res)
    (Ok (-1)) cols

let check_columns cols1 cols2 =
  List.fold_left
    (fun res x -> if List.mem x cols2 then res else Error `Invalid_column)
    (Ok ()) cols1

let append dt1 dt2 =
  let cols1 = (summary dt1).column_names
  and cols2 = (summary dt2).column_names in
  match (check_columns cols1 cols2, check_columns cols2 cols1) with
  | Ok (), Ok () ->
      List.fold_left
        (fun dt c ->
          match dt with
          | Ok dt ->
              let (Col s1) = get_col c dt1 |> Option.get in
              let (Col s2) = get_col c dt2 |> Option.get in
              add_col (Series.append s1 s2) dt
          | err -> err)
        (Ok (empty dt1.name))
        cols1
  | (Error `Invalid_column as err), _ | _, (Error `Invalid_column as err) -> err
