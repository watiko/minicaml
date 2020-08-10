module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax
module Eval = Minicaml.Eval

let value_testable = Alcotest.testable Syntax.pprint_value ( = )

let parse s =
  let result = Parser.(parse main (explode s)) in
  match result with
  | Some exp -> exp
  | None -> failwith @@ "parse error: " ^ s
;;

let test_plus () =
  let open Syntax in
  let open Eval in
  let table =
    [ "1 + 2", 3
    ; "-1 + 1", 0
    ; "0 + 10", 10
    ; "-1 + -2", -3
    ; "1 + 3 + (-10)", -6
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, value) ->
      Alcotest.(check value_testable)
        exp
        (IntVal value)
        (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_times () =
  let open Syntax in
  let open Eval in
  let table =
    [ "1 * 2", 2
    ; "100 * 0", 0
    ; "0 * -1", 0
    ; "-1 * -1", 1
    ; "1 * 2 * 3 * 4 * 5", 120
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, value) ->
      Alcotest.(check value_testable)
        exp
        (IntVal value)
        (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_math () =
  let open Syntax in
  let open Eval in
  let table =
    [ "(1 + 2) * (3 + 4)", 21
    ; "1 + 2 * 3 + 4", 11
    ; "(1 - 1) * 100", 0
    ; "(1 + (-3 - -10) * 10)", 71
    ; "20 / 4 * 2", 10
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, value) ->
      Alcotest.(check value_testable)
        exp
        (IntVal value)
        (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_eq () =
  let open Syntax in
  let open Eval in
  let table =
    [ "1 = 1", true
    ; "1 = 2", false
    ; "111 = 111", true
    ; "true = true", true
    ; "(100 + 1) = (50 + 51)", true
    ; "(1 * 10 * 100) = (100 * 1 * 10)", true
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, value) ->
      Alcotest.(check value_testable)
        exp
        (BoolVal value)
        (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_env () =
  let open Syntax in
  let open Eval in
  let env = emptyenv () in
  let env = ext env "a" (BoolVal false) in
  let env = ext env "x" (IntVal 1) in
  let env = ext env "y" (BoolVal true) in
  let env = ext env "a" (IntVal 100) in
  let table =
    [ "lookup from empty", "x", None, emptyenv ()
    ; "lookup x", "x", Some (IntVal 1), env
    ; "lookup y", "y", Some (BoolVal true), env
    ; "lookup latest a", "a", Some (IntVal 100), env
    ; "notfound", "notexist", None, env
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (name, var, want, env) ->
      let got = lookup var env in
      Alcotest.(check @@ option value_testable) name want got)
    table
;;

let test_let () =
  let open Syntax in
  let open Eval in
  let table =
    [ "let b = true in b", BoolVal true
    ; "let x = (3 * 11) in x", IntVal 33
    ; "let x = 10 in let y = 22 in x + y", IntVal 32
    ; "let x = 10 in let y = (100 / x) in y / x", IntVal 1
    ; "let x = 1 in let x = 2 in x", IntVal 2
    ; "let x = 1 in (let x = 2 in x + 1) + (x * 2)", IntVal 5
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, want) ->
      Alcotest.(check value_testable) exp want (eval (parse exp) @@ defaultenv ()))
    table
;;

let () =
  Alcotest.run
    "Eval"
    [ ( "eval"
      , [ Alcotest.test_case "plus" `Quick test_plus
        ; Alcotest.test_case "times" `Quick test_times
        ; Alcotest.test_case "math" `Quick test_math
        ; Alcotest.test_case "eq" `Quick test_eq
        ; Alcotest.test_case "env" `Quick test_env
        ; Alcotest.test_case "let" `Quick test_let
        ] )
    ]
;;
