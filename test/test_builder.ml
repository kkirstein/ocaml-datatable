(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable

(* Testable types *)
let table_summary =
  Alcotest.testable (Fmt.of_to_string Table.show_summary) ( = )

(* The tests *)
let colspec =
  let open Table in
  Row.empty
  |> Row.add "order" `String
  |> Row.add "values" `Float
  |> Row.add "count" `Int

type test_record = { order : string; values : float; count : int }

let record_to_row d =
  let open Table in
  let open Series in
  Row.empty
  |> Row.add "order" (DStr d.order)
  |> Row.add "values" (DFloat d.values)
  |> Row.add "count" (DInt d.count)

module MyBuilder = Builder.Make (struct
  type record = test_record

  let convert = record_to_row
end)

(* ---------------------------------------------------------------------- *)
let test_empty_table () =
  let b = MyBuilder.create "data" colspec in
  let t = MyBuilder.to_table b |> Result.get_ok in
  Alcotest.(check table_summary)
    "table summary"
    Table.
      {
        name = "data";
        num_rows = 0;
        column_names = [ "count"; "order"; "values" ];
      }
    (Table.summary t)

(* ---------------------------------------------------------------------- *)
let test_rows_added () =
  let b =
    MyBuilder.create "data" colspec
    |> MyBuilder.add { order = "eins"; values = 1.47; count = 3 }
    |> MyBuilder.add { order = "zwei"; values = 2.71; count = 2 }
    |> MyBuilder.add { order = "drei"; values = 3.14; count = 1 }
  in
  let t = MyBuilder.to_table b |> Result.get_ok in
  Alcotest.(check table_summary)
    "table summary"
    Table.
      {
        name = "data";
        num_rows = 3;
        column_names = [ "count"; "order"; "values" ];
      }
    (Table.summary t)

(* Test set *)
let test_set =
  [
    ("test empty table", `Quick, test_empty_table);
    ("test rows added", `Quick, test_rows_added);
  ]
