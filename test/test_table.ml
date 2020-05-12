(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable
open Datatable.Table

(* open Datatable.Series *)

(* Testable types *)
let summary_to_string sum =
  Printf.sprintf "{Table: %s; Rows: %d; Columns: %s}" sum.name sum.num_rows
    (String.concat ", " sum.column_names)

let table_summary = Alcotest.testable (Fmt.of_to_string summary_to_string) ( = )

let data_table =
  Alcotest.testable
    (Fmt.of_to_string (fun x -> summary x |> summary_to_string))
    ( = )

let column_to_string col =
  match col with
  | CFloat c -> Printf.sprintf "Float: %s" (Series.name c)
  | CInt c -> Printf.sprintf "Int: %s" (Series.name c)
  | CStr c -> Printf.sprintf "Str: %s" (Series.name c)

let data_column = Alcotest.testable (Fmt.of_to_string column_to_string) ( = )

let data_row_to_string row =
  match row with
  | Some r ->
      let data_to_string d =
        match d with
        | name, Series.DStr s -> name ^ ": " ^ s
        | name, Series.DFloat f -> name ^ ": " ^ string_of_float f
        | name, Series.DInt i -> name ^ ": " ^ string_of_int i
      in
      Row.bindings r |> List.map data_to_string |> String.concat "; "
  | None -> "None"

let data_row = Alcotest.testable (Fmt.of_to_string data_row_to_string) ( = )

(* The tests *)
let dt =
  let open Datatable.Series in
  empty "data"
  |> add_col (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
  |> add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
  |> add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))

(* ---------------------------------------------------------------------- *)
let test_empty_table () =
  let empty_dt = empty "data" in
  Alcotest.(check table_summary)
    "empty table"
    { name = "data"; num_rows = 0; column_names = [] }
    (summary empty_dt)

(* ---------------------------------------------------------------------- *)
let test_summary () =
  Alcotest.(check table_summary)
    "table summary"
    {
      name = "data";
      num_rows = 3;
      column_names = [ "count"; "values"; "order" ];
    }
    (summary dt)

(* ---------------------------------------------------------------------- *)
let test_get_col () =
  let exp_str =
    Series.(SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
  in
  let exp_float =
    Series.(SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
  in
  let exp_int = Series.(SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ])) in
  Alcotest.(check (option data_column))
    "get_col SStr" (Some (CStr exp_str)) (get_col "order" dt);
  Alcotest.(check (option data_column))
    "get_col SFloat" (Some (CFloat exp_float)) (get_col "values" dt);
  Alcotest.(check (option data_column))
    "get_col SInt" (Some (CInt exp_int)) (get_col "count" dt);
  Alcotest.(check (option data_column)) "get_col SInt" None (get_col "wrong" dt)

(* ---------------------------------------------------------------------- *)
let test_get_row () =
  (* let exp = Some (Row.of_seq (List.to_seq [
         "order", DStr "zwei"; "values", DFloat 2.71; "count", DInt 2]))
     in *)
  let exp =
    Some
      ( Row.empty
      |> Row.add "count" (Series.DInt 2)
      |> Row.add "values" (Series.DFloat 2.71)
      |> Row.add "order" (Series.DStr "zwei") )
  in
  let act = get_row 1 dt in
  print_endline (data_row_to_string act);
  Alcotest.(check data_row) "get_row" exp act

(* ---------------------------------------------------------------------- *)
let test_get_row_empty () =
  Alcotest.(check data_row)
    "get_row empty, no column" None (get_row ~names:[] 1 dt);
  Alcotest.(check data_row)
    "get_row empty, index out-of-bounds" None (get_row 3 dt);
  Alcotest.(check data_row)
    "get_row empty, invalid columns" None
    (get_row ~names:[ "eins"; "zwei" ] 1 dt)

(* ---------------------------------------------------------------------- *)

(* Test set *)
let test_set =
  [
    ("test empty table", `Quick, test_empty_table);
    ("test summary", `Quick, test_summary);
    ("test get_col", `Quick, test_get_col);
    ("test get_row", `Quick, test_get_row);
    ("test get_row empty", `Quick, test_get_row_empty);
  ]
