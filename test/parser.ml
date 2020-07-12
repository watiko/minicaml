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
  let pattern_test name expected input =
    Alcotest.(check (option exp_testable))
      name
      (Some expected)
      (parse pattern (explode input))
  in
  let cases_test name expected input =
    Alcotest.(check (option (list (pair exp_testable exp_testable))))
      name
      (Some expected)
      (parse cases (explode input))
  in
  pattern_test "var" (Var "x") "x";
  pattern_test "int" (IntLit 1) "1";
  pattern_test "empty" Empty "[]";
  pattern_test "list_with_literal" (Cons (IntLit 1, Empty)) "1 :: []";
  pattern_test
    "list_with_var"
    (Cons (Var "x", Cons (Var "y", Cons (Var "z", Empty))))
    "x :: y :: z ::[]";
  cases_test "case_one" [ IntLit 1, IntLit 100 ] "1 -> 100";
  cases_test
    "case_two"
    [ Empty, BoolLit true; Var "_", BoolLit false ]
    "| [] -> true | _ -> false"
;;

let test_match () =
  let open Parser in
  let open Syntax in
  let exp_test name expected input =
    Alcotest.(check (option exp_testable))
      name
      (Some expected)
      (parse exp (explode input))
  in
  exp_test "" (Match (Var "x", [ IntLit 1, IntLit 100 ])) "match x with 1 -> 100"
;;

let () =
  Alcotest.run
    "Parser"
    [ ( "parse"
      , [ Alcotest.test_case "tokens" `Quick test_tokens
        ; Alcotest.test_case "pattern" `Quick test_pattern
        ; Alcotest.test_case "match" `Quick test_match
        ] )
    ]
;;
