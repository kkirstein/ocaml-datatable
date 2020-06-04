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

let col_spec =
  let open Table in
  Row.empty
  |> Row.add "sepal_length" `Float
  |> Row.add "sepal_width" `Float
  |> Row.add "petal_length" `Float
  |> Row.add "petal_width" `Float
  |> Row.add "species" `String

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
let create_cols col_spec len dt =
  let open Series in
  let cols = Table.Row.bindings col_spec in
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

let fill_cols row_data dt =
  match
    List.fold_left
      (fun res r ->
        match res with Ok n -> Table.set_row r (n + 1) dt | Error _ as e -> e)
      (Ok (-1)) row_data
  with
  | Ok _ -> Ok dt
  | Error _ as e -> e

let from_row_list name rows =
  let ( >>= ) = Result.bind in
  let dt = Table.empty name in
  let len = List.length rows in
  if len > 0 then create_cols col_spec len dt >>= fill_cols rows else Ok dt

let summary_to_string sum =
  let open Table in
  Printf.sprintf "{Table: %s; Rows: %d; Columns: %s}" sum.name sum.num_rows
    (String.concat ", " sum.column_names)

let () =
  let tic = Sys.time () in
  Printf.printf "Loading data from CVS ..";
  let data = read_data "./examples/iris.csv" in
  let toc = Sys.time () in
  Printf.printf " done (%d records in %.3fs).\n" (List.length data) (toc -. tic);
  let tic = Sys.time () in
  Printf.printf "Importing data ..";
  let dt = from_row_list "iris" data |> Result.get_ok in
  let toc = Sys.time () in
  Printf.printf " done (%d rows in %.3fs):\n" (Table.length dt) (toc -. tic);
  Printf.printf "%s\n" (Table.summary dt |> summary_to_string)
