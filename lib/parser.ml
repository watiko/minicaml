open Peg.Core
open Syntax

(* === utils === *)

let parse = Peg.Core.parse
let explode = Peg.Utils.explode
let implode = Peg.Utils.implode
let rec fix f x = f (fix f) x

(* === tokens === *)

let trueP = string "true"
let falseP = string "false"
let funP = string "fun"
let letP = string "let"
let recP = string "rec"
let inP = string "in"
let ifP = string "if"
let thenP = string "then"
let elseP = string "else"
let matchP = string "match"
let withP = string "with"
let bool = trueP *> pure true <|> falseP *> pure false

let keyword =
  trueP
  <|> falseP
  <|> funP
  <|> letP
  <|> recP
  <|> inP
  <|> ifP
  <|> thenP
  <|> elseP
  <|> matchP
  <|> withP
;;

(* termination *)

let arrow = string "->"
let vbar = string "|"

(* group *)

let lparen = string "("
let rparen = string ")"
let lbra = string "["
let rbra = string "]"

(* op *)

let plus = string "+"
let minus = string "-"
let asterisk = string "*"
let slash = string "/"
let equal = string "="
let less = string "<"
let greater = string ">"
let semicol = string ";"
let colcol = string "::"
let infix0op = equal <|> less <|> greater
let infix1opR = colcol
let infix2op = plus <|> minus
let infix3op = asterisk <|> slash

(* literal *)

let var =
  let* _ = notP keyword in
  let* d = letter <|> char '_' in
  let* ds = many (alnum <|> char '_' <|> char '\'') in
  pure @@ Peg.Utils.implode (d :: ds)
;;

let int =
  let* neg = option false (minus *> pure true) in
  let* ns = many1 digit in
  let num = int_of_string @@ Peg.Utils.implode ns in
  let value = if neg then -num else num in
  pure value
;;

(* parser *)

let empty_list = lbra *> wss *> rbra *> pure Empty

let literalP =
  (fun v -> Var v)
  <$> var
  <|> ((fun i -> IntLit i) <$> int)
  <|> ((fun b -> BoolLit b) <$> bool)
  <|> empty_list
;;

let exp priexp cases =
  fix (fun exp ->
      let priexp = priexp exp in
      let matchP' =
        let* _ = token matchP in
        let* e = exp in
        let* _ = token withP in
        let* cs = cases in
        pure @@ Match (e, cs)
      in
      matchP' <|> priexp)
;;

let priexp exp = literalP <|> (lparen *> exp <* rparen)

let cases priexp pattern =
  fix (fun cases ->
      let exp = exp priexp cases in
      let single_pair =
        let* e1 = token pattern in
        let* _ = token arrow in
        let* e2 = token exp in
        pure (e1, e2)
      in
      let single = List.cons <$> single_pair <*> pure [] in
      let multiple = many1 (token vbar *> single_pair) in
      single <|> multiple)
;;

let pattern pattern_inner =
  fix (fun pattern ->
      let* p = token pattern_inner in
      let* ps = many (token colcol *> pattern) in
      match ps with
      | [] -> pure p
      | _ ->
        let ps = p :: ps in
        let r = Utils.fold_right1 (fun p acc -> Cons (p, acc)) ps in
        pure r)
;;

let pattern_inner = literalP
let pattern = pattern pattern_inner
let cases = cases priexp pattern
let exp = exp priexp cases
let priexp = priexp exp
