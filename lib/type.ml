open Syntax

type ty =
  | TInt
  | TBool
  | TString
  | TUnit

type tyenv = (string * ty) list

let emptytenv () : tyenv = []
let lookup = Eval.lookup
let ext = Eval.ext
let defaultenv = emptytenv

let type_name = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
;;

let pprint_type ppf t =
  let tname = type_name t in
  match t with
  | TInt | TBool | TString | TUnit -> Fmt.pf ppf "%s" tname
;;

let rec check e tenv =
  let ename = exp_name e in
  match e with
  | Var x ->
    (match lookup x tenv with
    | Some v -> v
    | None -> failwith @@ "lookup failed with key: " ^ x)
  | Unit -> TUnit
  | IntLit _ -> TInt
  | BoolLit _ -> TBool
  | StrLit _ -> TString
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
    binop ename e1 e2 TInt tenv
  | Greater (e1, e2) | Less (e1, e2) -> binop ename e1 e2 TBool tenv
  | Eq (e1, e2) ->
    let eqable t = List.mem t [ TUnit; TInt; TBool; TString ] in
    let t1 = check e1 tenv in
    let t2 = check e2 tenv in
    if t1 = t2 && eqable t1 then TBool else failwith "eq: can't compare"
  | If (c, e1, e2) ->
    (match check c tenv with
    | TBool ->
      let t1 = check e1 tenv in
      let t2 = check e2 tenv in
      if t1 = t2 then t1 else failwith "if: return type mismatch"
    | _ -> failwith "if: cond type is not bool")
  | _ -> failwith @@ "unknown type: " ^ exp_name e

and binop ename e1 e2 retType tenv =
  match check e1 tenv, check e2 tenv with
  | TInt, TInt -> retType
  | _ -> failwith @@ "type error: " ^ ename
;;
