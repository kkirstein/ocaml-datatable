(* vim: set ft=ocaml sw=2 ts=2: *)

(*
  Copyright (c) 2025 Kay-Uwe Kirstein, All rights reserved.
  Distributed under the MIT lincense.
 *)

(* Parse a float and return NaN for invalid format *)
(*let parse_float str =
  match float_of_string_opt str with Some f -> f | None -> Float.nan *)

let read_csv ~builder filepath =
  let module Csv_reader =
    (val builder : Builder.S with type row_t = string list)
  in
  let ci = open_in filepath in
  let csv = Csv.of_channel ~has_header:true ci in
  let table =
    Csv.fold_left
      ~f:(fun reader r -> Csv_reader.add r reader)
      ~init:(Csv_reader.create "iris") csv
  in
  Csv.close_in csv;
  Csv_reader.to_table table

let write_csv _filepath _dt = failwith "Not implemented"
