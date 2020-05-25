(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(**
    Load and analyse the iris flowers dataset.
*)

open Datatable

let parse_float str =
  match float_of_string_opt str with Some f -> f | None -> Float.nan

let row_from_strings strs =
  let open Table in
  let open Series in
  match strs with
  | [ _; sl; sw; pl; pw; spec ] ->
      Row.empty
      |> Row.add "sepal_length" (DFloat (parse_float sl))
      |> Row.add "sepal_width" (DFloat (parse_float sw))
      |> Row.add "petal_length" (DFloat (parse_float pl))
      |> Row.add "petal_width" (DFloat (parse_float pw))
      |> Row.add "species" (DStr spec)
  | _ -> Row.empty

(* read data from CSV file *)
let read_data path =
  let ci = open_in path in
  let csv = Csv.of_channel ~has_header:true ci in
  let data =
    Csv.fold_left ~f:(fun acc r -> row_from_strings r :: acc) ~init:[] csv
    |> List.fold_left
         (fun acc x -> if Table.Row.is_empty x then acc else x :: acc)
         []
  in
  Csv.close_in csv;
  data

(* convert row list to table *)
let create_cols : type a. int -> 'b Table.Row.t -> a Series.t list = fun len row ->
  let open Series in
  List.map (fun (k, v) -> match v with
  | DFloat _ -> SFloat (Floats.create ~name:k ~value:0.0 len)
  | DInt _   -> SInt (Ints.create ~name:k ~value:0 len)
  | DStr _   -> SStr (Strings.create ~name:k ~value:"" len)
) (Table.Row.bindings row)

let from_row_list name rows =
  let dt = Table.empty name in
  let len = List.length rows in
  if len > 0 then
    let _cols = create_cols len row in
    dt
  else dt

let () =
  let tic = Sys.time () in
  Printf.printf "Loading data from CVS ..";
  let data = read_data "./examples/iris.csv" in
  let toc = Sys.time () in
  Printf.printf " done (%d records in %.3fs).\n" (List.length data) (toc -. tic)
