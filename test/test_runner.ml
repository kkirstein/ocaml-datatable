(* vim: set ft=ocaml sw=2 ts=2: *)

let () = Alcotest.run "datatable" [
    "Table", Test_table.test_set
  ]

