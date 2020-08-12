open Utils

type 'a parser = char list -> ('a * char list) option

(* functor *)

let map f p cs =
  match p cs with
  | None -> None
  | Some (x, cs') -> Some (f x, cs')
;;

let ( <$> ) = map

(* applicative *)

let pure x cs = Some (x, cs)

let apply fp xp cs =
  match fp cs with
  | None -> None
  | Some (f, cs') -> (map f xp) cs'
;;

let ( <*> ) = apply
let ( <* ) xp yp = (fun x _ -> x) <$> xp <*> yp
let ( *> ) xp yp = (fun _ y -> y) <$> xp <*> yp
let product xp yp = (fun x y -> x, y) <$> xp <*> yp
let ( let+ ) x f = map f x
let ( and+ ) = product

(* monad *)

(* sequence: e1 e2 *)
let bind p f cs =
  match p cs with
  | None -> None
  | Some (x, cs') -> f x cs'
;;

let ( >>= ) = bind
let ( let* ) = bind

(* alternative *)
let either xp yp cs =
  match xp cs with
  | None -> yp cs
  | Some _ as x -> x
;;

let empty _ = None
let ( <|> ) = either

(* mplus *)
let mzero = empty

(* === primitives === *)

(* ordered choice: e1 / e2 *)
let ( </> ) = ( <|> )

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

let choice ps = List.fold_right ( <|> ) ps empty

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

let%test _ = Some ('a', [ 'b'; 'c' ]) = item @@ explode "abc"

let%test _ =
  let p =
    let* c1 = item in
    let* c2 = item in
    pure (implode [ c1; c2 ])
  in
  Some ("ab", [ 'c' ]) = p @@ explode "abc"
;;

let%test _ =
  let p =
    item >>= fun c1 ->
    item >>= fun c2 -> pure (implode [ c1; c2 ])
  in
  Some ("ab", [ 'c' ]) = p @@ explode "abc"
;;

let%test _ =
  let p = (fun x y -> implode [ x; y ]) <$> item <*> item in
  Some ("ab", [ 'c' ]) = p @@ explode "abc"
;;

let%test _ =
  let p =
    let+ c1 = item
    and+ c2 = item in
    implode [ c1; c2 ]
  in
  Some ("ab", [ 'c' ]) = p @@ explode "abc"
;;

let%test "middle" =
  let p = item *> item <* item in
  Some ('b', []) = p @@ explode "abc"
;;

let satisfy f =
  let* c = item in
  if f c then pure c else mzero
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
  explode s |> f |> map implode
;;

let%test "string" =
  let p = string "abc" in
  Some ("abc", []) = p @@ explode "abc"
;;

let token p = p <* wss

let%test "grammer" =
  let p = wss *> token item <* eof () in
  Some ('1', []) = p @@ explode " 1 "
;;
