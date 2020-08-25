let pp_parse_error ppf e = Fmt.pf ppf "%s" @@ Peg.Core.ParseError.show e
let pp_parser_state ppf s = Fmt.pf ppf "%s" @@ Peg.Core.ParserState.show s
let parse_error_testable = Alcotest.testable pp_parse_error ( = )
let parser_state_testable = Alcotest.testable pp_parser_state ( = )

let pp_parser_result a_show ppf r =
  let pp_a ppf a = Fmt.pf ppf "value(%s)" @@ a_show a in
  let pp_ok ppf v =
    Fmt.pf ppf "Ok: %a" (Fmt.pair ~sep:Fmt.comma pp_a pp_parser_state) v
  in
  let pp_err ppf e = Fmt.pf ppf "Error: %a" pp_parse_error e in
  Fmt.pf ppf "%a" (Fmt.result ~ok:pp_ok ~error:pp_err) r
;;

let parser_result_testable a_show = Alcotest.testable (pp_parser_result a_show) ( = )

let test_item_syntax () =
  let open Peg.Core in
  let open Peg.Utils in
  let open Parser.Syntax in
  let ps = [] in
  let ps =
    let p =
      let* c1 = item () in
      let* c2 = item () in
      pure (implode [ c1; c2 ])
    in
    ("monadic let", p) :: ps
  in
  let ps =
    let p =
      item () >>= fun c1 ->
      item () >>= fun c2 -> pure (implode [ c1; c2 ])
    in
    ("bind", p) :: ps
  in
  let ps =
    let p = (fun x y -> implode [ x; y ]) <$> item () <*> item () in
    ("applicative", p) :: ps
  in
  let ps =
    let p =
      let+ c1 = item ()
      and+ c2 = item () in
      implode [ c1; c2 ]
    in
    ("applicative let", p) :: ps
  in
  let testable = parser_result_testable (fun x -> x) in
  let want = Ok ("ab", ([ 'c' ], Pos.make 1 3)) in
  List.iter
    (fun (name, p) -> Alcotest.check testable name want @@ runParser p "abc")
    (List.rev ps)
;;

let test_basic_combinators () =
  let open Peg.Core in
  let open Peg.Utils in
  let open Parser.Syntax in
  let test a_show =
    let testable = parser_result_testable a_show in
    Alcotest.check testable
  in
  let testChar = test (fun c -> implode [ c ]) in
  let testUnit = test (fun () -> "unit") in
  let () =
    let p = item () in
    testChar "just item" (Ok ('a', ([ 'b'; 'c' ], Pos.make 1 2))) @@ runParser p "abc"
  in
  let () =
    let p = item () *> item () <* item () in
    testChar "middle" (Ok ('b', ([], Pos.make 1 4))) @@ runParser p "abc"
  in
  let () =
    let p = andP (char 'a') in
    testUnit "andP: success" (Ok ((), ([ 'a' ], Pos.make 1 1))) @@ runParser p "a";
    testUnit "andP: fail" (Error (ParseError.make "andP: fail" @@ Pos.make 1 1))
    @@ runParser p "b"
  in
  let () =
    let p = notP (char 'a') in
    testUnit "notP: success" (Ok ((), ([ 'b' ], Pos.make 1 1))) @@ runParser p "b";
    testUnit "notP: fail" (Error (ParseError.make "notP: fail" @@ Pos.make 1 1))
    @@ runParser p "a"
  in
  ()
;;

let test_string_basic () =
  let open Peg.Core in
  let open Peg.Utils in
  let open Parser.Syntax in
  let test a_show =
    let testable = parser_result_testable a_show in
    Alcotest.check testable
  in
  let testString = test (fun x -> x) in
  let testChar = test (fun c -> implode [ c ]) in
  let () =
    let p = string "abc" in
    testString "string" (Ok ("abc", ([], Pos.make 1 4))) @@ runParser p "abc"
  in
  let () =
    let p = wss *> token (item ()) <* eof () in
    testChar "grammer" (Ok ('1', ([], Pos.make 1 4))) @@ runParser p " 1 "
  in
  ()
;;

let () =
  Alcotest.run
    "Peg"
    [ "item", [ Alcotest.test_case "syntax" `Quick test_item_syntax ]
    ; "basic", [ Alcotest.test_case "combinators" `Quick test_basic_combinators ]
    ; "string", [ Alcotest.test_case "basic" `Quick test_string_basic ]
    ]
;;
