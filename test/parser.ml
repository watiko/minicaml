module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax

let exp_testable = Alcotest.testable Syntax.pprint_exp ( = )

let exp_test name expected input =
  let open Parser in
  Alcotest.(check (option exp_testable)) name (Some expected) (parse exp (explode input))
;;

let test_tokens () =
  let open Parser in
  let open Syntax in
  Alcotest.(check (option string))
    "not_keyword"
    (Some "inner")
    (parse var (explode "inner"));
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
  let open Syntax in
  exp_test "" (Match (Var "x", [ IntLit 1, IntLit 100 ])) "match x with 1 -> 100"
;;

let test_math () =
  let open Syntax in
  let i n = IntLit n in
  exp_test "plus" (Plus (i 1, i 2)) "1 + 2";
  exp_test "plusplus" (Plus (Plus (i 1, i 2), i 3)) "1 + 2 + 3";
  exp_test "timesdiv" (Div (Times (i 1, i 2), i 3)) "1 * 2 / 3";
  exp_test "eq" (Eq (i 1, i 2)) "1 = 2";
  exp_test
    "complex_math"
    (Div (Minus (Plus (i 1, Times (i 2, i 3)), i (-4)), i 10))
    "(1 + 2 * 3 - (-4)) / 10"
;;

let test_list () =
  let open Syntax in
  let i n = IntLit n in
  exp_test "empty" Empty "[]";
  exp_test "simple" (Cons (i 1, Empty)) "1 :: []";
  exp_test "simple long" (Cons (i 1, Cons (i 2, Cons (i 3, Empty)))) "1 :: 2 :: 3 :: []";
  exp_test "literal one" (Cons (i 1, Empty)) "[1]";
  exp_test "literal one'" (Cons (i 1, Empty)) "[1;]";
  exp_test "literal two" (Cons (i 1, Cons (i 2, Empty))) "[1; 2]";
  exp_test "literal two'" (Cons (i 1, Cons (i 2, Empty))) "[1; 2;]"
;;

let test_fn () =
  let open Syntax in
  exp_test "id" (Fun ("x", Var "x")) "fun x -> x"
;;

let test_let () =
  let open Syntax in
  exp_test "let_cmp" (Let ("x", IntLit 1, Eq (Var "x", IntLit 10))) "let x = 1 in x = 10";
  exp_test
    "let_rec"
    (LetRec ("fn", "_", IntLit 1, Eq (App (Var "fn", IntLit 2), IntLit 10)))
    "let rec fn _ = 1 in fn 2 = 10"
;;

let test_if () =
  let open Syntax in
  exp_test "simple" (If (BoolLit true, IntLit 1, IntLit 2)) "if true then 1 else 2"
;;

let () =
  Alcotest.run
    "Parser"
    [ ( "parse"
      , [ Alcotest.test_case "tokens" `Quick test_tokens
        ; Alcotest.test_case "pattern" `Quick test_pattern
        ; Alcotest.test_case "match" `Quick test_match
        ; Alcotest.test_case "math" `Quick test_math
        ; Alcotest.test_case "list" `Quick test_list
        ; Alcotest.test_case "fn" `Quick test_fn
        ; Alcotest.test_case "let" `Quick test_let
        ; Alcotest.test_case "if" `Quick test_if
        ] )
    ]
;;
