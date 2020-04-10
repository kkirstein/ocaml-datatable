(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable.Table
open Datatable.Series
(* open Datatable.Series *)

(* Testable types *)
let data_row_to_string row =
  match row with
  | Some r  -> (
  let data_to_string d = match d with
    | DStr s    -> s
    | DFloat f  -> string_of_float f
    | DInt i    -> string_of_int i
  in
  List.map data_to_string r |> String.concat "; ")
  | None    -> "None"

let data_row = Alcotest.testable (Fmt.of_to_string data_row_to_string) (=)

(* The tests *)
let dt = create "data" [
    Str ({name = "order"; data = [|"eins"; "zwei"; "drei"|]});
    Float ({name = "values";
            data = (Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|1.47; 2.71; 3.14|])});
    Int ({name = "count";
          data = (Bigarray.Array1.of_array Bigarray.int Bigarray.c_layout [|3; 2; 1|])})
  ]

(* ---------------------------------------------------------------------- *)
let test_get_row () =
  let exp = Some [DStr "zwei"; DFloat 2.71; DInt 2] in
  let act = get_row 1 dt in
  Alcotest.(check data_row) "get_row" exp act
(* ---------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------- *)

(* Test set *)
let test_set = [
  "test get_row", `Quick, test_get_row
]

