module C = Monad.Core
module StateT = C.StateT
module OptionT = C.OptionT
module EitherT = C.EitherT
module Identity = C.Identity

module IntType = struct
  type t = int
end

module IntListType = struct
  type t = int list
end

module Option = struct
  include OptionT.Make (Identity)

  let run m = Identity.run @@ runOptionT m
end

module IntState = struct
  include StateT.Make (IntType) (Identity)

  let run m s = Identity.run @@ runStateT m s
end

module StackState = struct
  include StateT.Make (IntListType) (Identity)

  let run m s = Identity.run @@ runStateT m s

  let push i =
    let open Syntax in
    let* s = get () in
    put (i :: s)
  ;;

  let pop () =
    let open Syntax in
    let* s = get () in
    let h, tl =
      match s with
      | h :: tl -> h, tl
      | [] -> failwith "StackState: pop"
    in
    let* _ = put tl in
    pure h
  ;;
end

module IntListState = struct
  include StateT.Make (IntListType) (IntState)

  let run m s1 s2 = IntState.run (runStateT m s1) s2
  let get_int () = lift @@ IntState.get ()
end

type myerror =
  | A of int
  | B of string
  | Fatal

let pprint_myerror ppf = function
  | A i -> Fmt.pf ppf "A(%d)" i
  | B s -> Fmt.pf ppf "B(%s)" s
  | Fatal -> Fmt.pf ppf "Fatal"
;;

let myerror_testable = Alcotest.testable pprint_myerror ( = )

module MyErrorResult = struct
  module T = struct
    type t = myerror
  end

  include EitherT.Make (T) (Identity)

  let run m = Identity.run @@ runEitherT m
end

let test_optiont_identity () =
  let module O = Option in
  let () =
    let v = O.run (O.from_option (Some 1)) in
    Alcotest.(check (option int)) "from_option" (Some 1) v
  in
  let () =
    let v = O.run @@ O.fold (O.from_option (Some 1)) (fun i -> i + 100) (fun () -> -1) in
    Alcotest.(check (option int)) "fold_some" (Some 101) v
  in
  let () =
    let v = O.run @@ O.fold (O.from_option None) (fun i -> i + 100) (fun () -> -1) in
    Alcotest.(check (option int)) "fold_none" (Some (-1)) v
  in
  let () =
    let v =
      O.run @@ O.foldF (O.pure 1) (fun i -> O.pure (i * 5)) (fun () -> O.pure (-1))
    in
    Alcotest.(check (option int)) "foldF" (Some 5) v
  in
  ()
;;

let test_statet_identity () =
  let module I = IntState in
  let () =
    let v = I.run (I.get ()) 111 in
    Alcotest.(check (pair int int)) "initial state" (111, 111) v
  in
  let () =
    let m =
      let open I.Syntax in
      let* s1 = I.get () in
      let* _ = I.put (s1 + 10) in
      let* s2 = I.get () in
      I.pure @@ (s2 * 10)
    in
    let v = I.run m 5 in
    Alcotest.(check (pair int int)) "get, put" (150, 15) v
  in
  ()
;;

let test_statet_nested () =
  let module I = IntState in
  let module L = IntListState in
  let () =
    let m =
      let open L.Syntax in
      let* ls1 = L.get () in
      let* is1 = L.get_int () in
      L.pure (ls1, is1)
    in
    let v = L.run m [ 10 ] 100 in
    Alcotest.(check (pair (list int) int)) "initial state" ([ 10 ], 100) @@ fst (fst v)
  in
  ()
;;

let test_statet_stackstate () =
  let module S = StackState in
  let () =
    let m =
      let open S.Syntax in
      let* _ = S.pop () in
      let* _ = S.pop () in
      let* _ = S.push 100 in
      S.push 101
    in
    let v = S.run m [ 1; 2; 3 ] in
    Alcotest.(check (list int)) "pop, push" [ 101; 100; 3 ] @@ snd v
  in
  ()
;;

let test_eithert_identity () =
  let module M = MyErrorResult in
  let () =
    let v = M.run (M.pure 1) in
    Alcotest.(check (result int myerror_testable)) "pure" (Ok 1) v
  in
  let () =
    let v = M.run @@ M.fold (M.pure 1) (fun s -> s * 100) (fun _ -> -1) in
    Alcotest.(check (result int myerror_testable)) "fold_right" (Ok 100) v
  in
  let () =
    let v = M.run @@ M.fold (M.throwError (B "fail")) (fun s -> s * 100) (fun _ -> -1) in
    Alcotest.(check (result int myerror_testable)) "fold_left" (Ok (-1)) v
  in
  let () =
    let v =
      M.run @@ M.bimap (M.throwError (B "fail")) (fun s -> s * 100) (fun _ -> Fatal)
    in
    Alcotest.(check (result int myerror_testable)) "bimap" (Error Fatal) v
  in
  let () =
    let v =
      M.run @@ M.foldF (M.pure 1) (fun s -> M.throwError (A s)) (fun _ -> M.pure 1)
    in
    Alcotest.(check (result int myerror_testable)) "foldF" (Error (A 1)) v
  in
  let () =
    let v = M.run (M.throwError (A 100)) in
    Alcotest.(check (result int myerror_testable)) "throw_1" (Error (A 100)) v
  in
  let () =
    let v = M.run (M.throwError Fatal) in
    Alcotest.(check (result int myerror_testable)) "throw_2" (Error Fatal) v
  in
  let () =
    let v =
      M.run
      @@ M.catchError (M.throwError (A 100)) (function
             | A i -> M.pure i
             | e -> M.throwError e)
    in
    Alcotest.(check (result int myerror_testable)) "raise_3" (Ok 100) v
  in
  ()
;;

let () =
  Alcotest.run
    "Monad"
    [ "OptionT", [ Alcotest.test_case "Identity" `Quick test_optiont_identity ]
    ; ( "StateT"
      , [ Alcotest.test_case "Identity" `Quick test_statet_identity
        ; Alcotest.test_case "Nested" `Quick test_statet_nested
        ; Alcotest.test_case "StackState" `Quick test_statet_stackstate
        ] )
    ; "EitherT", [ Alcotest.test_case "Identity" `Quick test_eithert_identity ]
    ]
;;
