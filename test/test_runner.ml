(* vim: set ft=ocaml sw=2 ts=2: *)

let () =
  Alcotest.run "datatable"
    [
      ("Series", Test_series.test_set);
      ("Table", Test_table.test_set);
      ("Builder", Test_builder.test_set);
    ]
