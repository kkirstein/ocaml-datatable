(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

module Row = Map.Make (String)


type column =
  | CFloat of float Series.t
  | CInt of int Series.t
  | CStr of string Series.t

type t = { name : string; columns : column list }

type row = Series.data_type Row.t

type summary = {
  name : string;
  num_rows : int;
  column_names : string list
}

let empty name = { name; columns = [] }

let add_col : type a. a Series.t -> t -> t = fun s dt ->
  let open Series in
  let col = match s with
    | SFloat _ as c  -> CFloat c
    | SInt _ as c    -> CInt c
    | SStr _ as c    -> CStr c
  in
  {dt with columns = col :: dt.columns}

(*
let summary dt =
  match dt.columns with
  | []  -> {name = dt.name; num_rows = 0; column_names = []}
  | (_c :: _) as cs  -> {name = dt.name; num_rows = Series.length c; column_names = List.map Series.name cs}

let create name data = { name; columns = data }

let get_row ?names idx dt =
  let cols =
    match names with
    | None -> List.map (fun x -> Series.name x) dt.columns
    | Some n -> n
  in
  let row =
    match cols with
    | [] -> Row.empty
    | _ -> (
        try
          List.fold_left
            (fun acc c ->
              let cname = Series.name c in
              if List.mem cname cols then Row.add cname (Series.get idx c) acc
              else acc)
            Row.empty dt.columns
        with _ -> Row.empty )
  in
  if Row.is_empty row then None else Some row

let get_col name dt =
  match List.filter (fun c -> Series.name c = name) dt.columns with
  | [] -> None
  | cs -> Some (List.hd cs)
   *)

