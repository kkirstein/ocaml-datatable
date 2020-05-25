(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2020 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(**
    Load and analyse the iris flowers dataset.
*)

type row_data = {
  sepal_length : float;
  sepal_width : float;
  petal_width : float;
  petal_length : float;
  species : string;
}

let parse_float str =
  match float_of_string_opt str with Some f -> f | None -> Float.nan

let row_from_strings strs =
  match strs with
  | [ _; sl; sw; pw; pl; spec ] ->
      Some
        {
          sepal_width = parse_float sw;
          sepal_length = parse_float sl;
          petal_width = parse_float pw;
          petal_length = parse_float pl;
          species = spec;
        }
  | _ -> None

(* rad data from CSV file *)
let read_data path =
  let ci = open_in path in
  let csv = Csv.of_channel ~has_header:true ci in
  let data =
    Csv.fold_left ~f:(fun acc r -> (row_from_strings r) :: acc) ~init:[] csv
    |> List.fold_left (fun acc x ->
           match x with Some r -> r :: acc | None -> acc) []
  in
  Csv.close_in csv;
  data

let () =
let tic = Sys.time () in
  Printf.printf "Loading data from CVS ..";
  let data = read_data "./examples/iris.csv" in
  let toc = Sys.time () in
  Printf.printf " done (%d records in %.3fs).\n" (List.length data) (toc -. tic)
