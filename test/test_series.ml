(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable.Series

(* Testable types *)
let summary_to_string sum =
  Printf.sprintf "{name: %s; type: %s; length: %d}" sum.name sum.data_type
    sum.length

let data_series_to_string s =
  let sum = summary s in
  summary_to_string sum

let data_series =
  Alcotest.testable (Fmt.of_to_string data_series_to_string) ( = )

let data_summary = Alcotest.testable (Fmt.of_to_string summary_to_string) ( = )

(* The tests *)
let s_ints = SInt (Ints.from_list ~name:"count" [ 3; 2; 1; 0 ])

let s_strs = SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ])

let s_floats = SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ])

(* ---------------------------------------------------------------------- *)
let test_summary () =
  Alcotest.(check data_summary)
    "summary of data series"
    { name = "count"; data_type = "int"; length = 4 }
    (summary s_ints);
  Alcotest.(check data_summary)
    "summary of data series"
    { name = "values"; data_type = "float"; length = 3 }
    (summary s_floats);
  Alcotest.(check data_summary)
    "summary of data series"
    { name = "order"; data_type = "string"; length = 3 }
    (summary s_strs)

(* Test set *)
let test_set = [ ("test summary", `Quick, test_summary) ]
