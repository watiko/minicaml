module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax
module Eval = Minicaml.Eval

let value_testable = Alcotest.testable Syntax.pprint_value ( = )
let parse = Eval.unsafeParse

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

let test_cmp () =
  let open Syntax in
  let open Eval in
  let table =
    [ "1 > 1", false
    ; "2 > 1", true
    ; "1 < 10", true
    ; "-1 < 10", true
    ; "2 < 2", false
    ; "5 > 10", false
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

let test_func () =
  let open Syntax in
  let open Eval in
  let table =
    [ "fun x -> x", FunVal ("x", Var "x", defaultenv ())
    ; "fun _ -> 100", FunVal ("_", IntLit 100, defaultenv ())
    ; ( "let outer = 1 in let id = fun x -> x in id"
      , FunVal ("x", Var "x", ext (defaultenv ()) "outer" (IntVal 1)) )
    ; "let outer = 2 in let f = fun _ -> outer in f ()", IntVal 2
    ; "let id = fun x -> x in id 100", IntVal 100
    ; "let add = fun x -> fun y -> x + y in add 100 11", IntVal 111
    ; "let x = 1 in let f = fun y -> x + y in let x = 2 in f (x + 3)", IntVal 6
    ]
  in
  List.iter
    (fun (exp, want) ->
      Alcotest.(check value_testable) exp want (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_letrec () =
  let open Syntax in
  let open Eval in
  let table =
    [ "let rec f x = x in f", RecFunVal ("f", "x", Var "x", defaultenv ())
    ; ( {|
let fact = fun n ->
  let rec inner n = fun acc ->
    if n = 0
      then acc
      else inner (n - 1) (acc * n)
  in inner n 1
in fact 5|}
      , IntVal 120 )
    ; "let x = 1 in let f = fun y -> x + y in let x = 2 in f (x + 3)", IntVal 6
    ]
  in
  List.iter
    (fun (exp, want) ->
      Alcotest.(check value_testable) exp want (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_list () =
  let open Syntax in
  let open Eval in
  let table =
    [ "List.hd [1; 2; 3]", IntVal 1
    ; "List.tl [1; 2; 3]", ListVal [ IntVal 2; IntVal 3 ]
    ; "List.hd [[1]; [2]]", ListVal [ IntVal 1 ]
    ; "[1; 2] = [1; 2]", BoolVal true
    ; "[2; 1] = [1; 2]", BoolVal false
    ; "[1] = [1; 2]", BoolVal false
    ; "[true] = [true]", BoolVal true
    ]
  in
  List.iter
    (fun (exp, want) ->
      Alcotest.(check value_testable) exp want (eval (parse exp) @@ defaultenv ()))
    table
;;

let test_match () =
  let open Syntax in
  let open Eval in
  let table =
    [ "match 1 with 1 -> true", BoolVal true
    ; "match 3 with | 1 -> 10 | 2 -> 20 | 3 -> 30", IntVal 30
    ; "match [1] with | 1 :: [] -> true | _ -> false", BoolVal true
    ; "match [] with | 1 :: [] -> true | _ -> false", BoolVal false
    ; "match [1; 2; 3] with 1 :: 2 :: 3 :: [] -> true", BoolVal true
    ; "match 1 with | 0 -> -1 | 100 -> -1 | i -> i", IntVal 1
    ; "match [1] with h :: [] -> h", IntVal 1
    ; "match [1; 2; 3] with | _ :: [] -> 1 | _ :: _ -> 2", IntVal 2
    ; "match [1; 2; 3] with h :: _  -> h", IntVal 1
    ; "match [1; 2; 3] with _ :: tl -> tl", ListVal [ IntVal 2; IntVal 3 ]
    ]
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
        ; Alcotest.test_case "cmp" `Quick test_cmp
        ; Alcotest.test_case "let" `Quick test_let
        ; Alcotest.test_case "func" `Quick test_func
        ; Alcotest.test_case "letrec" `Quick test_letrec
        ; Alcotest.test_case "list" `Quick test_list
        ; Alcotest.test_case "match" `Quick test_match
        ] )
    ; "env", [ Alcotest.test_case "env" `Quick test_env ]
    ]
;;
