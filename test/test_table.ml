(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable.Table
open Datatable.Series

(* open Datatable.Series *)

(* Testable types *)
let data_row_to_string row =
  match row with
  | Some r ->
      let data_to_string d =
        match d with
        | name, DStr s -> name ^ ": " ^ s
        | name, DFloat f -> name ^ ": " ^ string_of_float f
        | name, DInt i -> name ^ ": " ^ string_of_int i
      in
      Row.bindings r |> List.map data_to_string |> String.concat "; "
  | None -> "None"

let data_row = Alcotest.testable (Fmt.of_to_string data_row_to_string) ( = )

let data_series_to_string s =
  let sum = summary s in
  Printf.sprintf "{name: %s; type: %s; length: %d}" sum.name sum.data_type
    sum.length

let data_series =
  Alcotest.testable (Fmt.of_to_string data_series_to_string) ( = )

(* The tests *)
let dt =
  create "data"
    [
      SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]);
      SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]);
      SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]);
    ]

(* ---------------------------------------------------------------------- *)
let test_get_row () =
  (* let exp = Some (Row.of_seq (List.to_seq [
         "order", DStr "zwei"; "values", DFloat 2.71; "count", DInt 2]))
     in *)
  let exp =
    Some
      ( Row.empty
      |> Row.add "order" (DStr "zwei")
      |> Row.add "values" (DFloat 2.71)
      |> Row.add "count" (DInt 2) )
  in
  let act = get_row 1 dt in
  Alcotest.(check data_row) "get_row" exp act

(* ---------------------------------------------------------------------- *)
let test_get_row_empty () =
  Alcotest.(check data_row) "get_row empty" None (get_row ~names:[] 1 dt);
  Alcotest.(check data_row) "get_row empty" None (get_row 3 dt);
  Alcotest.(check data_row)
    "get_row empty" None
    (get_row ~names:[ "eins"; "zwei" ] 1 dt)

(* ---------------------------------------------------------------------- *)
let test_get_col () =
  let exp_str =
    SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ])
  in
  let exp_float =
    SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ])
  in
  let exp_int = SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]) in
  Alcotest.(check (option data_series))
    "get_col SStr" (Some exp_str) (get_col "order" dt);
  Alcotest.(check (option data_series))
    "get_col SFloat" (Some exp_float) (get_col "values" dt);
  Alcotest.(check (option data_series))
    "get_col SInt" (Some exp_int) (get_col "count" dt);
  Alcotest.(check (option data_series)) "get_col SInt" None (get_col "wrong" dt)

(* ---------------------------------------------------------------------- *)

(* Test set *)
let test_set =
  [
    ("test get_row", `Quick, test_get_row);
    ("test get_row empty", `Quick, test_get_row_empty);
    ("test get_col", `Quick, test_get_col);
  ]
