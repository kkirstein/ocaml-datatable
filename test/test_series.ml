(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable.Series

(* Testable types *)
let data_type_to_string d =
  match d with
  | DInt i -> "DInt: " ^ string_of_int i
  | DFloat f -> "DFloat: " ^ string_of_float f
  | DStr s -> "DStr: " ^ s

let data_series_to_string : type a. a t -> string =
 fun s -> summary s |> show_summary

let data_type = Alcotest.testable (Fmt.of_to_string data_type_to_string) ( = )

let data_series_int : int t Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string data_series_to_string) ( = )

let data_series_float : float t Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string data_series_to_string) ( = )

let data_series_string : string t Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string data_series_to_string) ( = )

let data_summary = Alcotest.testable (Fmt.of_to_string show_summary) ( = )

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

(* ---------------------------------------------------------------------- *)
let test_length () =
  Alcotest.(check int) "length of data series" 4 (length s_ints)

(* ---------------------------------------------------------------------- *)
let test_from_list () =
  Alcotest.(check data_series_int)
    "from_list -> SInt" s_ints
    (SInt (Ints.from_list ~name:"count" [ 3; 2; 1; 0 ]));
  Alcotest.(check data_series_float)
    "from_list -> SFloat" s_floats
    (SFloat (Floats.from_list ~name:"values" [ 1.47; 2.71; 3.14 ]));
  Alcotest.(check data_series_string)
    "from_list -> SStr" s_strs
    (SStr (Strings.from_list ~name:"order" [ "eins"; "zwei"; "drei" ]))

(* ---------------------------------------------------------------------- *)
let test_get () =
  Alcotest.(check int) "test get first" 3 (get 0 s_ints);
  Alcotest.(check int) "test get last" 0 (get 3 s_ints);
  Alcotest.check_raises "test get out-of-bound"
    (Invalid_argument "index out of bounds") (fun () -> ignore (get 4 s_ints))

(* ---------------------------------------------------------------------- *)
let test_set () =
  let () = set 1 42 s_ints in
  Alcotest.(check int) "test set" 42 (get 1 s_ints)

(* Test set *)
let test_set =
  [
    ("test summary", `Quick, test_summary);
    ("test length", `Quick, test_length);
    ("test from_list", `Quick, test_from_list);
    ("test get", `Quick, test_get);
    ("test set", `Quick, test_set);
  ]
