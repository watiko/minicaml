open Syntax

type ty =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of ty * ty
  | TVar of string

let new_typevar s = TVar ("'" ^ s)

type tyenv = (string * ty) list

let emptytenv () : tyenv = []
let lookup = Eval.lookup
let ext = Eval.ext
let defaultenv = emptytenv

let substitute tvar t tenv =
  List.map (fun (x, t') -> if t' = tvar then x, t else x, t') tenv
;;

let type_name = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow _ -> "fun"
  | TVar _ -> "tvar"
;;

let rec pprint_type ppf t =
  let tname = type_name t in
  match t with
  | TInt | TBool | TString | TUnit -> Fmt.pf ppf "%s" tname
  | TArrow (t1, t2) ->
    Fmt.pf ppf "@[<v 2>%s@ %a->@ %a@]" tname pprint_type t1 pprint_type t2
  | TVar x -> Fmt.pf ppf "@[<v 2>%s@ %s@]" tname x
;;

let rec infer tenv e =
  let ename = exp_name e in
  match e with
  | Var x ->
    (match lookup x tenv with
    | Some t -> tenv, t
    | None ->
      let tvar = new_typevar x in
      let tenv = ext tenv x tvar in
      tenv, tvar)
  | Unit -> tenv, TUnit
  | IntLit _ -> tenv, TInt
  | BoolLit _ -> tenv, TBool
  | StrLit _ -> tenv, TString
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
    binop_infer tenv e1 e2 ename TInt
  | Greater (e1, e2) | Less (e1, e2) -> binop_infer tenv e1 e2 ename TBool
  | Eq (e1, e2) ->
    let eqable t = List.mem t [ TUnit; TInt; TBool; TString ] in
    let tenv, t1 = infer tenv e1 in
    let tenv, t2 = infer tenv e2 in
    let tenv =
      match t1, t2 with
      | _, _ when t1 = t2 && eqable t1 -> tenv
      | TVar _, TVar _ ->
        let tenv = substitute t1 t2 tenv in
        tenv
      | _, TVar _ when eqable t1 ->
        let tenv = substitute t2 t1 tenv in
        tenv
      | TVar _, _ when eqable t2 ->
        let tenv = substitute t1 t2 tenv in
        tenv
      | _ -> failwith "eq: type mismatch"
    in
    tenv, TBool
  | If (c, e1, e2) ->
    let tenv, ct = infer tenv c in
    let tenv = require tenv ct ename TBool in
    let tenv, t1 = infer tenv e1 in
    let tenv, t2 = infer tenv e2 in
    (match t1, t2 with
    | _, _ when t1 = t2 -> tenv, t1
    | TVar _, TVar _ ->
      let tenv = substitute t1 t2 tenv in
      tenv, t2
    | _, TVar _ ->
      let tenv = substitute t2 t1 tenv in
      tenv, t1
    | TVar _, _ ->
      let tenv = substitute t1 t2 tenv in
      tenv, t2
    | _ -> failwith "if: type mismatch")
  | _ -> failwith @@ "infer: not implemented: " ^ ename

and require tenv t ename wantType =
  match t with
  | TVar _ as tvar -> substitute tvar wantType tenv
  | _ when t = wantType -> tenv
  | _ -> failwith @@ "type error: " ^ ename

and binop_infer tenv e1 e2 ename retType =
  let tenv, t1 = infer tenv e1 in
  let tenv = require tenv t1 ename TInt in
  let tenv, t2 = infer tenv e2 in
  let tenv = require tenv t2 ename TInt in
  tenv, retType
;;

let rec check e tenv =
  let lookup x tenv =
    match lookup x tenv with
    | Some v -> v
    | None -> failwith @@ "lookup failed with key: " ^ x
  in
  let ename = exp_name e in
  match e with
  | Var x -> lookup x tenv
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
  | Fun (x, e1) ->
    let t1 = lookup x tenv in
    let t2 = check e1 tenv in
    TArrow (t1, t2)
  | App (e1, e2) ->
    let fun_t = check e1 tenv in
    let arg_t = check e2 tenv in
    (match fun_t with
    | TArrow (from_t, to_t) ->
      if arg_t = from_t then to_t else failwith "app: arg type mismatch"
    | _ -> failwith "app: fun type required")
  | Let (x, e1, e2) ->
    let t1 = check e1 tenv in
    let tenv = ext tenv x t1 in
    check e2 tenv
  | _ -> failwith @@ "unknown type: " ^ exp_name e

and binop ename e1 e2 retType tenv =
  match check e1 tenv, check e2 tenv with
  | TInt, TInt -> retType
  | _ -> failwith @@ "type error: " ^ ename
;;
