(* vim: set ft=ocaml sw=2 ts=2: *)

let () = Alcotest.run "datatable" [
    "Datatable", Test_datatable.test_set
  ]

