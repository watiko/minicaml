module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax

let exp_testable = Alcotest.testable Syntax.pprint_exp ( = )

let test_tokens () =
  let open Parser in
  let open Syntax in
  Alcotest.(check (option string))
    "var(complex)"
    (Some "a_a'0a")
    (parse var (explode "a_a'0a"));
  Alcotest.(check (option string)) "var(_)" (Some "_") (parse var (explode "_"));
  Alcotest.(check (option int)) "int(plus)" (Some 123) (parse int (explode "123"));
  Alcotest.(check (option exp_testable))
    "empty_list"
    (Some Empty)
    (parse empty_list (explode "[ \n ]"));
  Alcotest.(check (option int)) "int(minus)" (Some (-123)) (parse int (explode "-123"))
;;

let test_pattern () =
  let open Parser in
  let open Syntax in
  let exp_test name expected input =
    Alcotest.(check (option exp_testable))
      name
      (Some expected)
      (parse pattern (explode input))
  in
  exp_test "var" (Var "x") "x";
  exp_test "empty" Empty "[]";
  exp_test "list_with_literal" (Cons (IntLit 1, Empty)) "1 :: []";
  exp_test
    "list_with_var"
    (Cons (Var "x", Cons (Var "y", Cons (Var "z", Empty))))
    "x :: y :: z ::[]"
;;

let () =
  Alcotest.run
    "Parser"
    [ ( "parse"
      , [ Alcotest.test_case "tokens" `Quick test_tokens
        ; Alcotest.test_case "pattern" `Quick test_pattern
        ] )
    ]
;;
