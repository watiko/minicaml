module Syntax = Minicaml.Syntax
module Eval = Minicaml.Eval

let value_testable = Alcotest.testable Syntax.pprint_value ( = )

let test_plus () =
  let open Syntax in
  let open Eval in
  let table =
    [ "1 + 2", Plus (IntLit 1, IntLit 2), IntVal 3
    ; "-1 + 1", Plus (IntLit (-1), IntLit 1), IntVal 0
    ; "0 + 10", Plus (IntLit 0, IntLit 10), IntVal 10
    ; "-1 + -2", Plus (IntLit (-1), IntLit (-2)), IntVal (-3)
    ]
  in
  List.iter
    (fun (name, exp, value) -> Alcotest.(check value_testable) name (eval exp) value)
    table
;;

let test_times () =
  let open Eval in
  Alcotest.(check value_testable) "times" (eval (Times (IntLit 1, IntLit 2))) (IntVal 2)
;;

let () =
  Alcotest.run
    "Eval"
    [ ( "eval"
      , [ Alcotest.test_case "plus" `Quick test_plus
        ; Alcotest.test_case "times" `Quick test_times
        ] )
    ]
;;
