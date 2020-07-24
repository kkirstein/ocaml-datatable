(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(**
    Load and analyse the iris flowers dataset.
*)

open Datatable

(* column specification *)

(* convert single record *)
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

module Csv_reader = Builder.Make (struct
  type record = string list

  let colspec =
    let open Table in
    Row.empty
    |> Row.add "sepal_length" `Float
    |> Row.add "sepal_width" `Float
    |> Row.add "petal_length" `Float
    |> Row.add "petal_width" `Float
    |> Row.add "species" `String

  let convert = row_from_strings
end)

(* read data from CSV file *)
let read_data path =
  let ci = open_in path in
  let csv = Csv.of_channel ~has_header:true ci in
  let table =
    Csv.fold_left
      ~f:(fun reader r -> Csv_reader.add r reader)
      ~init:(Csv_reader.create "iris") csv
  in
  Csv.close_in csv;
  Csv_reader.to_table table

let summary_to_string sum =
  let open Table in
  Printf.sprintf "{Table: %s; Rows: %d; Columns: %s}" sum.name sum.num_rows
    (String.concat ", " sum.column_names)

let () =
  let tic = Sys.time () in
  Printf.printf "Loading data from CVS ..";
  let dt = read_data "./examples/iris.csv" |> Result.get_ok in
  let toc = Sys.time () in
  Printf.printf " done (%d records in %.3fs).\n" (Table.length dt) (toc -. tic);
  Printf.printf "%s\n" (Table.summary dt |> summary_to_string)
