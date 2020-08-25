open Utils
module MC = Monad.Core
module Monad = MC.Monad

module Pos = struct
  type t =
    { line : int
    ; column : int
    }

  let make line column = { line; column }
  let initialPos = { line = 1; column = 1 }

  let updatePosChar pos c =
    match c with
    | '\n' | 'r' -> { line = pos.line + 1; column = 1 }
    | _ -> { pos with column = pos.column + 1 }
  ;;

  let updatePosString pos s =
    let chars = explode s in
    List.fold_left updatePosChar pos chars
  ;;

  let show pos = Fmt.str "Pos { line: %d, column: %d }" pos.line pos.column
end

module ParseError = struct
  type t = string * Pos.t

  let make message pos : t = message, pos
  let errorMessage (message, _) : string = message
  let errorPos (_, pos) : Pos.t = pos
  let empty () = "", Pos.initialPos
  let show (mes, pos) = Fmt.str "ParseError: %s at %s" mes (Pos.show pos)
end

module ParseResult = struct
  include MC.EitherT.Make (ParseError) (MC.Identity)

  let run m = MC.Identity.run @@ runEitherT m
  let empty () : 'a m = Error (ParseError.empty ())

  let either mx my =
    match mx () with
    | Ok x -> Ok x
    | Error _ -> my ()
  ;;
end

module ParserState = struct
  type t = char list * Pos.t

  let show (cs, pos) = Fmt.str "%s\n%s" (Pos.show pos) (implode cs)
end

module Parser = struct
  include MC.StateT.Make (ParserState) (ParseResult)

  let run m s = ParseResult.run @@ runStateT m s
  let foldF m fa fb : 'a m = fun s -> ParseResult.foldF (m s) fa fb
  let either xp yp : 'a m = fun s -> ParseResult.either (fun () -> xp s) (fun () -> yp s)
  let empty () : 'a m = fun _ -> ParseResult.empty ()
  let throwError e : 'a m = lift @@ ParseResult.throwError e

  (* state op *)
  open Syntax

  let getBuffer () : 'a m = fst <$> get ()
  let getPos () : 'a m = snd <$> get ()
  let putBuffer cs : 'a m = update (fun (_, pos) -> cs, pos)
  let putPos pos : 'a m = update (fun (cs, _) -> cs, pos)

  let fail message =
    let* pos = getPos () in
    throwError (ParseError.make message pos)
  ;;
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
  let* s = Parser.get () in
  let* pos = Parser.getPos () in
  Parser.foldF
    p
    (fun _ -> ParseResult.pure ((), s))
    (fun _ -> ParseResult.throwError @@ ParseError.make "andP: fail" pos)
;;

(* not-predicate:  !e *)
let notP (p : 'a parser) : unit parser =
  let* s = Parser.get () in
  let* pos = Parser.getPos () in
  Parser.foldF
    p
    (fun _ -> ParseResult.throwError @@ ParseError.make "notP: fail" pos)
    (fun _ -> ParseResult.pure ((), s))
;;

(* derived *)

let choice ps = List.fold_right ( <|> ) ps (empty ())

(* utils *)

let runParser p s = Parser.run p (explode s, Pos.initialPos)

let parse p cs =
  let r = Parser.run p (cs, Pos.initialPos) in
  match r with
  | Error _ -> None
  | Ok (x, _) -> Some x
;;

(* parser *)

let item () =
  let* cs = Parser.getBuffer () in
  let* pos = Parser.getPos () in
  match cs with
  | [] -> Parser.fail "item: no buffer left"
  | c :: cs' ->
    let* _ = Parser.putBuffer cs' in
    let* _ = Parser.putPos (Pos.updatePosChar pos c) in
    Parser.pure c
;;

let satisfy f =
  let* c = item () in
  if f c then pure c else empty ()
;;

let eof x =
  let* cs = Parser.getBuffer () in
  match cs with
  | [] -> pure x
  | _ -> Parser.fail "expected eof"
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

let token p = p <* wss
