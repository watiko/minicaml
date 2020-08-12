module Parser = Minicaml.Parser
module Syntax = Minicaml.Syntax
module Eval = Minicaml.Eval
module Type = Minicaml.Type

let type_testable = Alcotest.testable Type.pprint_type ( = )
let parse = Eval.unsafeParse

let test_literals () =
  let open Type in
  let table =
    [ "1", TInt
    ; "true", TBool
    ; "()", TUnit
    ; {|"aaaa"|}, TString
    ] [@ocamlformat "disable"]
  in
  List.iter
    (fun (exp, t) -> Alcotest.(check type_testable) exp t (check (parse exp)))
    table
;;

let test_int_binop () =
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
  List.iter
    (fun (exp, t) -> Alcotest.(check type_testable) exp t (check (parse exp)))
    table
;;

let test_if () =
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
  List.iter
    (fun (exp, t) ->
      let got =
        try Some (check (parse exp)) with
        | _ -> None
      in
      Alcotest.(check (option type_testable)) exp t got)
    table
;;

let () =
  Alcotest.run
    "Type"
    [ ( "check"
      , [ Alcotest.test_case "literals" `Quick test_literals
        ; Alcotest.test_case "int_binop" `Quick test_int_binop
        ; Alcotest.test_case "if" `Quick test_if
        ] )
    ]
;;
