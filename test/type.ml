module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax
module Eval = Minicaml.Eval
module Type = Minicaml.Type

let type_testable = Alcotest.testable Type.pprint_type ( = )
let parse = Eval.unsafeParse

type kind =
  | Check
  | Infer

let test_var kind () =
  let open Type in
  let tenv = defaultenv () in
  let tenv = ext tenv "x" TInt in
  let tenv = ext tenv "y" TBool in
  let tenv = ext tenv "z" TString in
  let tenv = ext tenv "x" TUnit in
  let table =
    [ "x", TUnit, tenv
    ; "y", TBool, tenv
    ; "z", TString, tenv
    ] [@ocamlformat "disable"]
  in
  if kind = Check
  then
    List.iter
      (fun (exp, t, tenv) ->
        Alcotest.(check type_testable) exp t (check (parse exp) tenv))
      table
  else
    List.iter
      (fun (exp, t, tenv) ->
        let _, got = infer tenv (parse exp) in
        Alcotest.(check type_testable) exp t got)
      table
;;

let test_literals kind () =
  let open Type in
  let table =
    [ "1", TInt
    ; "true", TBool
    ; "()", TUnit
    ; {|"aaaa"|}, TString
    ] [@ocamlformat "disable"]
  in
  if kind = Check
  then
    List.iter
      (fun (exp, t) ->
        Alcotest.(check type_testable) exp t (check (parse exp) @@ defaultenv ()))
      table
  else
    List.iter
      (fun (exp, t) ->
        let _, got = infer (defaultenv ()) (parse exp) in
        Alcotest.(check type_testable) exp t got)
      table
;;

let test_int_binop kind () =
  let open Type in
  let table =
    [ "1 + 2", TInt
    ; "-1 - 1", TInt
    ; "0 * 10", TInt
    ; "-1 / -2", TInt
    ; "1 > 20", TBool
    ; "1 < 20", TBool
    ; "1 = 1", TBool
    ]
  in
  if kind = Check
  then
    List.iter
      (fun (exp, t) ->
        Alcotest.(check type_testable) exp t (check (parse exp) @@ defaultenv ()))
      table
  else (
    let table = List.concat [ table; [ "x = y + 1", TBool ] ] in
    List.iter
      (fun (exp, t) ->
        let _, got = infer (defaultenv ()) (parse exp) in
        Alcotest.(check type_testable) exp t got)
      table)
;;

let test_if kind () =
  let open Type in
  let table =
    [ "if 1 then 1 else 1", None
    ; "if () then 1 else 1", None
    ; "if \"\" then 1 else 1", None
    ; "if (fun x -> x) then 1 else 1", None
    ; "if true then 1 else ()", None
    ; "if true then () else 1", None
    ; "if true then \"\" else true", None
    ; "if true then 1 else 1", Some TInt
    ; "if true then () else ()", Some TUnit
    ; "if true then \"\" else \"\"", Some TString
    ; "if true then true else false", Some TBool
    ]
  in
  if kind = Check
  then
    List.iter
      (fun (exp, t) ->
        let got =
          try Some (check (parse exp) @@ defaultenv ()) with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table
  else (
    let table = List.concat [ table; [ "if true then x else 100", Some TInt ] ] in
    List.iter
      (fun (exp, t) ->
        let got =
          try
            let _, got = infer (defaultenv ()) (parse exp) in
            Some got
          with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table)
;;

let test_fun kind () =
  let open Type in
  let etenv = defaultenv () in
  let tenv = etenv in
  let tenv = ext tenv "print" @@ TArrow (TString, TUnit) in
  let table =
    [ {|print "hello"|}, Some TUnit, tenv
    ; ( "fun x -> if true then x else 100"
      , Some (TArrow (TInt, TInt))
      , if kind = Check then ext etenv "x" TInt else etenv )
    ; ( "(fun x -> if true then x else 100) (if true then y else 200)"
      , Some TInt
      , ext (ext etenv "x" TInt) "y" TInt )
    ; ( "fun f -> (fun x -> f (f (f x + 10)))"
      , Some (TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)))
      , let tenv = ext etenv "f" @@ TArrow (TInt, TInt) in
        let tenv = ext tenv "x" @@ TInt in
        tenv )
    ]
  in
  if kind = Check
  then
    List.iter
      (fun (exp, t, tenv) ->
        let got =
          try Some (check (parse exp) tenv) with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table
  else (
    let table =
      List.concat
        [ table
        ; [ "(fun x -> x) true", Some TBool, defaultenv ()
          ; "(fun x -> x + 1)", Some (TArrow (TInt, TInt)), defaultenv ()
          ]
        ]
    in
    List.iter
      (fun (exp, t, tenv) ->
        let got =
          try
            let _, got = infer tenv (parse exp) in
            Some got
          with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table)
;;

let test_let kind () =
  let open Type in
  let etenv = defaultenv () in
  let tenv = etenv in
  let table = [ "let x = 1 in x", Some TInt, tenv ] in
  if kind = Check
  then
    List.iter
      (fun (exp, t, tenv) ->
        let got =
          try Some (check (parse exp) tenv) with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table
  else
    List.iter
      (fun (exp, t, tenv) ->
        let got =
          try
            let _, got = infer tenv (parse exp) in
            Some got
          with
          | _ -> None
        in
        Alcotest.(check (option type_testable)) exp t got)
      table
;;

let test_letrec () =
  let open Type in
  let table =
    [ "let rec id x = x in id 1", Some TInt
    ; "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5", Some TInt
    ; ( {|
      let rec fact n = fun k ->
        if n = 0 then k 1
        else fact (n - 1) (fun x ->
             k (x * n))
      in fact 5 (fun x -> x)
      |}
      , Some TInt )
    ]
  in
  List.iter
    (fun (exp, t) ->
      let got =
        try
          let _, got = infer (defaultenv ()) (parse exp) in
          Some got
        with
        | e ->
          print_string (Printexc.to_string e);
          None
      in
      Alcotest.(check (option type_testable)) exp t got)
    table
;;

let () =
  Alcotest.run
    "Type"
    [ ( "check"
      , [ Alcotest.test_case "var" `Quick @@ test_var Check
        ; Alcotest.test_case "literals" `Quick @@ test_literals Check
        ; Alcotest.test_case "int_binop" `Quick @@ test_int_binop Check
        ; Alcotest.test_case "if" `Quick @@ test_if Check
        ; Alcotest.test_case "fun" `Quick @@ test_fun Check
        ; Alcotest.test_case "let" `Quick @@ test_let Check
        ] )
    ; ( "infer"
      , [ Alcotest.test_case "var" `Quick @@ test_var Infer
        ; Alcotest.test_case "literals" `Quick @@ test_literals Infer
        ; Alcotest.test_case "int_binop" `Quick @@ test_int_binop Infer
        ; Alcotest.test_case "if" `Quick @@ test_if Infer
        ; Alcotest.test_case "fun" `Quick @@ test_fun Infer
        ; Alcotest.test_case "let" `Quick @@ test_let Infer
        ; Alcotest.test_case "letrec" `Quick test_letrec
        ] )
    ]
;;
