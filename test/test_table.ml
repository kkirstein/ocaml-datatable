(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable
open Datatable.Table

(* Testable types *)
let table_summary = Alcotest.testable (Fmt.of_to_string show_summary) ( = )

let data_table =
  Alcotest.testable
    (Fmt.of_to_string (fun x -> summary x |> show_summary))
    ( = )

let column_to_string col =
  let (Col s) = col in
  match s with
  | Series.SFloat _ -> Printf.sprintf "Float: %s" (Series.name s)
  | Series.SInt _ -> Printf.sprintf "Int: %s" (Series.name s)
  | Series.SStr _ -> Printf.sprintf "Str: %s" (Series.name s)

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

let data_table_result :
    ( t,
      [ `Invalid_index | `Invalid_datatype | `Invalid_column | `Invalid_length ]
    )
    result
    Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string Error.result_to_string) ( = )

let data_row_result :
    ( int,
      [ `Invalid_index | `Invalid_datatype | `Invalid_column | `Invalid_length ]
    )
    result
    Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string Error.result_to_string) ( = )

(* The tests *)
let ( >>= ) = Result.bind

(* ---------------------------------------------------------------------- *)
let test_empty_table () =
  let empty_dt = empty "data" in
  Alcotest.(check int) "empty table has zero rows" 0 (length empty_dt);
  Alcotest.(check table_summary)
    "empty table"
    { name = "data"; num_rows = 0; column_names = [] }
    (summary empty_dt)

(* ---------------------------------------------------------------------- *)
let test_invalid_length () =
  let dt_result =
    empty "data"
    |> add_col
         (SStr
            (Series.Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
  in
  Alcotest.(check bool) "length matches" true (dt_result |> Result.is_ok);
  let invalid_result =
    Result.bind dt_result
      (add_col
         (SStr
            (Series.Strings.from_list ~name:"order"
               [ "eins"; "zwei"; "drei"; "vier" ])))
  in
  Alcotest.(check data_table_result)
    "length mismatch"
    (Error `Invalid_length)
    invalid_result

(* ---------------------------------------------------------------------- *)
let test_summary () =
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  Alcotest.(check table_summary)
    "table summary"
    {
      name = "data";
      num_rows = 3;
      column_names = [ "count"; "order"; "values" ];
    }
    (summary dt)

(* ---------------------------------------------------------------------- *)
let test_get_col () =
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let exp_str =
    Series.(SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
  in
  let exp_float =
    Series.(SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
  in
  let exp_int = Series.(SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ])) in
  Alcotest.(check (option data_column))
    "get_col SStr" (Some (Col exp_str)) (get_col "order" dt);
  Alcotest.(check (option data_column))
    "get_col SFloat" (Some (Col exp_float)) (get_col "values" dt);
  Alcotest.(check (option data_column))
    "get_col SInt" (Some (Col exp_int)) (get_col "count" dt);
  Alcotest.(check (option data_column)) "get_col SInt" None (get_col "wrong" dt)

(* ---------------------------------------------------------------------- *)
let test_get_row () =
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
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
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  Alcotest.(check data_row)
    "get_row empty, no column" None (get_row ~names:[] 1 dt);
  Alcotest.(check data_row)
    "get_row empty, index out-of-bounds" None (get_row 3 dt);
  Alcotest.(check data_row)
    "get_row empty, invalid columns" None
    (get_row ~names:[ "eins"; "zwei" ] 1 dt)

(* ---------------------------------------------------------------------- *)
let test_set_row () =
  let open Datatable.Series in
  let dt =
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let row_data =
    Row.empty
    |> Row.add "order" (DStr "zwölf")
    |> Row.add "count" (DInt 13)
    |> Row.add "values" (DFloat 17.4)
  in
  let exp_dt =
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwölf"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 17.4; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 13; 1 ]))
    |> Result.get_ok
  in
  Alcotest.(check data_row_result) "set row" (Ok 1) (set_row row_data 1 dt);
  Alcotest.(check data_table) "set row side effect" exp_dt dt

(* ---------------------------------------------------------------------- *)
let test_set_row_partial () =
  let open Datatable.Series in
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let row_data =
    Row.empty
    |> Row.add "order" (DStr "zwölf")
    |> Row.add "values" (DFloat 17.4)
  in
  let exp_dt =
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwölf"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 17.4; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  Alcotest.(check data_row_result) "set row" (Ok 1) (set_row row_data 1 dt);
  Alcotest.(check data_table) "set row side effect" exp_dt dt

(* ---------------------------------------------------------------------- *)
let test_set_row_invalid () =
  let dt =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let row_data =
    Row.empty
    |> Row.add "order" (Series.DStr "zwölf")
    |> Row.add "count" (Series.DInt 13)
    |> Row.add "values" (Series.DFloat 17.4)
  in
  let row_data_invalid_column =
    Row.empty
    |> Row.add "order" (Series.DStr "zwölf")
    |> Row.add "count2" (Series.DInt 13)
    |> Row.add "values" (Series.DFloat 17.4)
  in
  let row_data_invalid_type =
    Row.empty
    |> Row.add "order" (Series.DStr "zwölf")
    |> Row.add "count" (Series.DInt 13)
    |> Row.add "values" (Series.DInt 17)
  in
  Alcotest.(check data_row_result)
    "set row invalid column"
    (Error `Invalid_column)
    (set_row row_data_invalid_column 1 dt);
  Alcotest.(check data_row_result)
    "set row invalid index"
    (Error `Invalid_index)
    (set_row row_data 3 dt);
  Alcotest.(check data_row_result)
    "set row invalid data type"
    (Error `Invalid_datatype)
    (set_row row_data_invalid_type 1 dt)

(* ---------------------------------------------------------------------- *)
let test_append () =
  let dt1 =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let dt2 =
    let open Datatable.Series in
    Ok (empty "data2")
    >>= add_col (SStr (Strings.from_list ~name:"order" [ "vier"; "fünf" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2 ]))
    |> Result.get_ok
  in
  Alcotest.(check table_summary)
    "append tables"
    {
      name = "data";
      num_rows = 5;
      column_names = [ "count"; "order"; "values" ];
    }
    (Table.append dt1 dt2 |> Result.get_ok |> Table.summary)

(* ---------------------------------------------------------------------- *)
let test_append_invalid () =
  let dt1 =
    let open Datatable.Series in
    Ok (empty "data")
    >>= add_col
          (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2; 1 ]))
    |> Result.get_ok
  in
  let dt2 =
    let open Datatable.Series in
    Ok (empty "data2")
    >>= add_col (SStr (Strings.from_list ~name:"order" [ "vier"; "fünf" ]))
    >>= add_col (SFloat (Floats.from_list ~name:"value" [ 1.47; 2.71 ]))
    >>= add_col (SInt (Ints.from_list ~name:"count" [ 3; 2 ]))
    |> Result.get_ok
  in
  Alcotest.(check data_table_result)
    "append tables failed"
    (Error `Invalid_column)
    (Table.append dt1 dt2)

(* Test set *)
let test_set =
  [
    ("test empty table", `Quick, test_empty_table);
    ("test invalid length", `Quick, test_invalid_length);
    ("test summary", `Quick, test_summary);
    ("test get_col", `Quick, test_get_col);
    ("test get_row", `Quick, test_get_row);
    ("test get_row empty", `Quick, test_get_row_empty);
    ("test set_row", `Quick, test_set_row);
    ("test set_row partial", `Quick, test_set_row_partial);
    ("test set_row invalid", `Quick, test_set_row_invalid);
    ("test append", `Quick, test_append);
    ("test append failed", `Quick, test_append_invalid);
  ]
