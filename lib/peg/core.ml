open Utils
module MC = Monad.Core
module Monad = MC.Monad

module ParseResult = struct
  include MC.OptionT.Make (MC.Identity)

  let run m = MC.Identity.run @@ runOptionT m
end

module Parser = struct
  include MC.StateT.Make
            (struct
              type t = char list
            end)
            (ParseResult)

  let run m s = ParseResult.run @@ runStateT m s
  let either xp yp s = ParseResult.either (xp s) (yp s)
  let empty () _s = ParseResult.empty ()
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
let andP p cs =
  match p cs with
  | None -> None
  | Some _ -> Some ((), cs)
;;

(* not-predicate:  !e *)
let notP p cs =
  match p cs with
  | None -> Some ((), cs)
  | Some _ -> None
;;

(* derived *)

let choice ps = List.fold_right ( <|> ) ps (empty ())

(* utils *)

let parse p cs =
  match p cs with
  | None -> None
  | Some (x, _) -> Some x
;;

(* parser *)

let item cs =
  match cs with
  | [] -> None
  | c :: cs' -> Some (c, cs')
;;

let%test _ = Some ('a', [ 'b'; 'c' ]) = Parser.run item @@ explode "abc"

let%test _ =
  let p =
    let* c1 = item in
    let* c2 = item in
    pure (implode [ c1; c2 ])
  in
  Some ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p =
    item >>= fun c1 ->
    item >>= fun c2 -> pure (implode [ c1; c2 ])
  in
  Some ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p = (fun x y -> implode [ x; y ]) <$> item <*> item in
  Some ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test _ =
  let p =
    let+ c1 = item
    and+ c2 = item in
    implode [ c1; c2 ]
  in
  Some ("ab", [ 'c' ]) = Parser.run p @@ explode "abc"
;;

let%test "middle" =
  let p = item *> item <* item in
  Some ('b', []) = Parser.run p @@ explode "abc"
;;

let satisfy f =
  let* c = item in
  if f c then pure c else empty ()
;;

let eof x = function
  | [] -> Some (x, [])
  | _ -> None
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
  Some ("abc", []) = Parser.run p @@ explode "abc"
;;

let token p = p <* wss

let%test "grammer" =
  let p = wss *> token item <* eof () in
  Some ('1', []) = Parser.run p @@ explode " 1 "
;;
