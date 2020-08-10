open Peg.Core
open Syntax

(* === utils === *)

let parse = Peg.Core.parse
let explode = Peg.Utils.explode
let implode = Peg.Utils.implode
let rec fix f x = f (fix f) x

(* === tokens === *)

module Token = struct
  type infix0op =
    | Equal
    | Less
    | Greater

  type infix11opR = Colcol

  type infix2op =
    | Plus
    | Minus

  type infix3op =
    | Asterisk
    | Slash
end

let idP =
  let* d = letter <|> char '_' in
  let* ds = many (alnum <|> char '.' <|> char '_' <|> char '\'') in
  pure @@ Peg.Utils.implode (d :: ds)
;;

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
  let px =
    [ trueP
    ; falseP
    ; funP
    ; letP
    ; recP
    ; inP
    ; ifP
    ; thenP
    ; elseP
    ; matchP
    ; withP
    ] [@ocamlformat "disable"]
  in
  let px = List.map (fun p -> p <* notP idP) px in
  choice px
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

let infix0op =
  let open Token in
  equal *> pure Equal <|> less *> pure Less <|> greater *> pure Greater
;;

let infix1opR = colcol *> pure Token.Colcol

let infix2op =
  let open Token in
  plus *> pure Plus <|> minus *> pure Minus
;;

let infix3op =
  let open Token in
  asterisk *> pure Asterisk <|> slash *> pure Slash
;;

(* literal *)

let var = notP keyword *> idP

let int =
  let* neg = option false (minus *> pure true) in
  let* ns = many1 digit in
  let num = int_of_string @@ Peg.Utils.implode ns in
  let value = if neg then -num else num in
  pure value
;;

let empty_list = lbra *> wss *> rbra *> pure Empty
let unitP = lparen *> wss *> rparen *> pure Unit

(* parser *)

let exp prefix cases =
  fix (fun exp ->
      let funP' =
        let* _ = token funP in
        let* x = token var in
        let* _ = token arrow in
        let* e = token exp in
        pure @@ Fun (x, e)
      in
      let letP' =
        let* _ = token letP in
        let* x = token var in
        let* _ = token equal in
        let* e1 = token exp in
        let* _ = token inP in
        let* e2 = token exp in
        pure @@ Let (x, e1, e2)
      in
      let letRecP =
        let* _ = token letP in
        let* _ = token recP in
        let* n = token var in
        let* x = token var in
        let* _ = token equal in
        let* e1 = token exp in
        let* _ = token inP in
        let* e2 = token exp in
        pure @@ LetRec (n, x, e1, e2)
      in
      let ifP' =
        let* _ = token ifP in
        let* ce = token exp in
        let* _ = token thenP in
        let* e1 = token exp in
        let* _ = token elseP in
        let* e2 = token exp in
        pure @@ If (ce, e1, e2)
      in
      let matchP' =
        let* _ = token matchP in
        let* e = token exp in
        let* _ = token withP in
        let* cs = token cases in
        pure @@ Match (e, cs)
      in
      choice [ prefix; funP'; letP'; letRecP; ifP'; matchP' ])
;;

let prefix infix0 = infix0

let infix0 infix1 =
  let pair a b = a, b in
  let f e1 (op, e2) =
    match op with
    | Token.Equal -> Eq (e1, e2)
    | Token.Less -> Less (e1, e2)
    | Token.Greater -> Greater (e1, e2)
  in
  let* e1 = token infix1 in
  let* es = many (pair <$> token infix0op <*> token infix1) in
  pure @@ List.fold_left f e1 es
;;

let infix1 infix2 =
  fix (fun infix1 ->
      let pair a b = a, b in
      let f (op, e1) e2 =
        match op with
        | Token.Colcol -> Cons (e2, e1)
      in
      let* e1 = token infix2 in
      let* es = many (pair <$> token infix1opR <*> token infix1) in
      pure @@ List.fold_right f es e1)
;;

let infix2 infix3 =
  let pair a b = a, b in
  let f e1 (op, e2) =
    match op with
    | Token.Plus -> Plus (e1, e2)
    | Token.Minus -> Minus (e1, e2)
  in
  let* e1 = token infix3 in
  let* es = many (pair <$> token infix2op <*> token infix3) in
  pure @@ List.fold_left f e1 es
;;

let infix3 infix4 =
  let pair a b = a, b in
  let f e1 (op, e2) =
    match op with
    | Token.Asterisk -> Times (e1, e2)
    | Token.Slash -> Div (e1, e2)
  in
  let* e1 = token infix4 in
  let* es = many (pair <$> token infix3op <*> token infix4) in
  pure @@ List.fold_left f e1 es
;;

let infix4 priexp =
  let apply =
    let* fn = token priexp in
    let* es = many1 (token priexp) in
    pure @@ List.fold_left (fun e1 e2 -> App (e1, e2)) fn es
  in
  apply <|> priexp
;;

let exp_from priexp cases =
  let infix4 = infix4 priexp in
  let infix3 = infix3 infix4 in
  let infix2 = infix2 infix3 in
  let infix1 = infix1 infix2 in
  let infix0 = infix0 infix1 in
  let prefix = prefix infix0 in
  let exp = exp prefix cases in
  exp
;;

let priexp cases literalP =
  fix (fun priexp ->
      let exp = exp_from priexp cases in
      literalP <|> (lparen *> token exp <* rparen))
;;

let cases pattern literalP =
  fix (fun cases ->
      let priexp = priexp cases literalP in
      let exp = exp_from priexp cases in
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
      let* ps = many (token colcol *> token pattern) in
      match ps with
      | [] -> pure p
      | _ ->
        let ps = p :: ps in
        let r = Utils.fold_right1 (fun p acc -> Cons (p, acc)) ps in
        pure r)
;;

let pattern_inner literalP = literalP

let literalP list =
  choice
    [ (fun v -> Var v) <$> var
    ; (fun i -> IntLit i) <$> int
    ; (fun b -> BoolLit b) <$> bool
    ; unitP
    ; empty_list
    ; list
    ]
;;

let list =
  fix (fun list ->
      let literalP = literalP list in
      let pattern_inner = pattern_inner literalP in
      let pattern = pattern pattern_inner in
      let cases = cases pattern literalP in
      let priexp = priexp cases literalP in
      let exp = exp_from priexp cases in
      let* _ = token lbra in
      let* e = token exp in
      let* es = many (token semicol *> token exp) in
      let* _ = optional @@ token semicol in
      let* _ = token rbra in
      match es with
      | [] -> pure @@ Cons (e, Empty)
      | _ ->
        let es = e :: es in
        let r = List.fold_right (fun e acc -> Cons (e, acc)) es Empty in
        pure r)
;;

(* fix *)

let literalP = literalP list
let pattern_inner = pattern_inner literalP
let pattern = pattern pattern_inner
let cases = cases pattern literalP
let priexp = priexp cases literalP
let infix4 = infix4 priexp
let infix3 = infix3 infix4
let infix2 = infix2 infix3
let infix1 = infix1 infix2
let infix0 = infix0 infix1
let prefix = prefix infix0
let exp = exp prefix cases

(* grammer *)

let main = wss *> token exp <* eof ()
