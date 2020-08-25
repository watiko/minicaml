open Utils
module MC = Monad.Core
module Monad = MC.Monad

module ParseResult = struct
  module T = struct
    type t = string
  end

  include MC.EitherT.Make (T) (MC.Identity)

  let run m = MC.Identity.run @@ runEitherT m

  let empty () : 'a m =
    let mempty = "" in
    Error mempty
  ;;

  let either mx my =
    let mappend x y = x ^ " : " ^ y in
    let append_left e r = Result.map_error (fun v -> mappend v e) r in
    match mx with
    | Ok x -> Ok x
    | Error e -> (append_left e) my
  ;;
end

module Parser = struct
  module T = struct
    type t = char list
  end

  include MC.StateT.Make (T) (ParseResult)

  let run m s = ParseResult.run @@ runStateT m s
  let foldF m fa fb : 'a m = fun s -> ParseResult.foldF (m s) fa fb
  let either xp yp : 'a m = fun s -> ParseResult.either (xp s) (yp s)
  let empty () : 'a m = fun _ -> ParseResult.empty ()
  let throwError e : 'a m = lift @@ ParseResult.throwError e

  (* state op *)
  let getBuffer () : 'a m = get ()
  let putBuffer s : 'a m = put s
end

open Parser.Syntax

type 'a parser = 'a Parser.m

(* ordered choice: e1 / e2 *)
let ( <|> ) = Parser.either
let empty _ = Parser.empty ()

(* zero-or-more: e* *)
let rec many p cs = (many1 p <|> pure []) cs
(* one-or-more: e+ *)
and many1 p cs = (List.cons <$> p <*> many p) cs

let option default p = p <|> pure default

(* optional: e? *)
let optional p = option None (p >>= fun x -> pure @@ Some x)

(* and-predicate:  &e *)
let andP (p : 'a parser) : unit parser =
  let* s = Parser.getBuffer () in
  Parser.foldF
    p
    (fun _ -> ParseResult.pure ((), s))
    (fun _ -> ParseResult.throwError "andP: fail")
;;

(* not-predicate:  !e *)
let notP (p : 'a parser) : unit parser =
  let* s = Parser.get () in
  Parser.foldF
    p
    (fun _ -> ParseResult.throwError "notP: fail")
    (fun _ -> ParseResult.pure ((), s))
;;

(* derived *)

let choice ps = List.fold_right ( <|> ) ps (empty ())

(* utils *)

let parse p cs =
  let r = Parser.run p cs in
  match r with
  | Error _ -> None
  | Ok (x, _) -> Some x
;;

(* parser *)

let item () =
  let* cs = Parser.getBuffer () in
  match cs with
  | [] -> Parser.throwError "item: no buffer left"
  | c :: cs' -> Parser.putBuffer cs' >>= fun _ -> Parser.pure c
;;

let%test _ = Ok ('a', [ 'b'; 'c' ]) = Parser.run (item ()) @@ explode "abc"

let%test _ =
  let p =
    let* c1 = item () in
    let* c2 = item () in
    pure (implode [ c1; c2 ])
  in
  Ok ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p =
    item () >>= fun c1 ->
    item () >>= fun c2 -> pure (implode [ c1; c2 ])
  in
  Ok ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p = (fun x y -> implode [ x; y ]) <$> item () <*> item () in
  Ok ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p =
    let+ c1 = item ()
    and+ c2 = item () in
    implode [ c1; c2 ]
  in
  Ok ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test "middle" =
  let p = item () *> item () <* item () in
  Ok ('b', []) = Parser.run p @@ explode "abc"
;;

let satisfy f =
  let* c = item () in
  if f c then pure c else empty ()
;;

let eof x =
  let* cs = Parser.getBuffer () in
  match cs with
  | [] -> pure x
  | _ -> Parser.throwError "expected eof"
;;

let char c = satisfy (( = ) c)
let range l r = satisfy (fun c -> l <= c && c <= r)
let one_of cs = satisfy (fun c -> List.mem c cs)
let none_of cs = satisfy (fun c -> not @@ List.mem c cs)

(* char *)

let lower = range 'a' 'z'
let upper = range 'A' 'Z'
let digit = range '0' '9'
let letter = lower <|> upper
let alnum = letter <|> digit
let ws = one_of [ ' '; '\t'; '\r'; '\n' ]
let wss = many ws

(* string *)

let string s =
  let rec f = function
    | [] -> pure []
    | c :: cs -> char c *> f cs *> pure (c :: cs)
  in
  explode s |> f |> fmap implode
;;

let%test "string" =
  let p = string "abc" in
  Ok ("abc", []) = Parser.run p @@ explode "abc"
;;

let token p = p <* wss

let%test "grammer" =
  let p = wss *> token (item ()) <* eof () in
  Ok ('1', []) = Parser.run p @@ explode " 1 "
;;
