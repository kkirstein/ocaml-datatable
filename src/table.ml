(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)


module Row = Map.Make(String)
type row = Series.data_type Row.t

type t = {
  name : string;
  columns : Series.t list
}

let create name data =
  {name; columns = data}

let get_row ?names idx dt =
  let cols =  match names with
    | None    -> List.map (fun x -> Series.name x) dt.columns
    | Some n  -> n
  in
  let row = match cols with
    | []  -> Row.empty
    | _   -> try
        List.fold_left
          (fun acc c -> let cname = Series.name c in
            if List.mem cname cols 
            then Row.add cname (Series.get idx c) acc
            else acc)
          Row.empty dt.columns
      with
        _ -> Row.empty
  in
  if Row.is_empty row then None else Some row

let get_col name dt =
match List.filter (fun c -> (Series.name c) = name) dt.columns with
  | []  -> None
  | cs  -> Some (List.hd cs)

