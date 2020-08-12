open Syntax

type typ =
  | TInt
  | TBool
  | TString
  | TUnit

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

let rec check e =
  let ename = exp_name e in
  match e with
  | Unit -> TUnit
  | IntLit _ -> TInt
  | BoolLit _ -> TBool
  | StrLit _ -> TString
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
    binop ename e1 e2 TInt
  | Greater (e1, e2) | Less (e1, e2) -> binop ename e1 e2 TBool
  | Eq (e1, e2) ->
    let eqable t = List.mem t [ TUnit; TInt; TBool; TString ] in
    let t1 = check e1 in
    let t2 = check e2 in
    if t1 = t2 && eqable t1 then TBool else failwith "eq: can't compare"
  | If (c, e1, e2) ->
    (match check c with
    | TBool ->
      let t1 = check e1 in
      let t2 = check e2 in
      if t1 = t2 then t1 else failwith "if: return type mismatch"
    | _ -> failwith "if: cond type is not bool")
  | _ -> failwith @@ "unknown type: " ^ exp_name e

and binop ename e1 e2 retType =
  match check e1, check e2 with
  | TInt, TInt -> retType
  | _ -> failwith @@ "type error: " ^ ename
;;
