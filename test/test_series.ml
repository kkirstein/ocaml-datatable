(* vim: set ft=ocaml sw=2 ts=2: *)

open Datatable
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

let data_unit_result : (int, [ `Invalid_index ]) result Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string Error.result_to_string) ( = )

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
  let act = get 1 s_floats in
  Alcotest.(check bool) "test get float" true (2.71 = act);
  Alcotest.check_raises "test get out-of-bound"
    (Invalid_argument "index out of bounds") (fun () -> ignore (get 4 s_ints))

(* ---------------------------------------------------------------------- *)
let test_set () =
  Alcotest.(check data_unit_result) "test set" (Ok 1) (set 1 42 s_ints);
  Alcotest.(check int) "test set value" 42 (get 1 s_ints);
  Alcotest.(check data_unit_result)
    "test invalid index"
    (Error `Invalid_index)
    (set 4 42 s_ints)

(* ---------------------------------------------------------------------- *)
let test_append () =
  let s_int_append = append s_ints s_ints |> Result.get_ok in
  let s_float_append = append s_floats s_floats |> Result.get_ok in
  (* let s_str_append = append s_strs s_strs |> Result.get_ok in *)
  Alcotest.(check int) "length of appended int series" 8 (length s_int_append);
  Alcotest.(check bool)
    "content of appended int series" true
    (get 0 s_int_append = get 4 s_int_append);
  Alcotest.(check bool)
    "content of appended int series" true
    (get 3 s_int_append = get 7 s_int_append);
  Alcotest.(check int)
    "length of appended float series" 6 (length s_float_append);
  Alcotest.(check bool)
    "content of appended float series" true
    (get 0 s_float_append = get 3 s_float_append);
  Alcotest.(check bool)
    "content of appended float series" true
    (get 2 s_float_append = get 5 s_float_append)

(* Alcotest.(check int) "length of appended str series" 6 (length s_str_append);
   Alcotest.(check string)
        "content of appended str series" (get 0 s_str_append) (get 3 s_str_append);
      Alcotest.(check string)
        "content of appended str series" (get 2 s_str_append) (get 5 s_str_append) *)

(* Test set *)
let test_set =
  [
    ("test summary", `Quick, test_summary);
    ("test length", `Quick, test_length);
    ("test from_list", `Quick, test_from_list);
    ("test get", `Quick, test_get);
    ("test set", `Quick, test_set);
    ("test append", `Quick, test_append);
  ]
