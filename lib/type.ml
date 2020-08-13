open Syntax

type tyvar = string

type ty =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of ty * ty
  | TVar of tyvar

let new_typevar n = TVar ("'a" ^ string_of_int n), n + 1

type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let emptytenv () : tyenv = []
let lookup = Eval.lookup
let ext = Eval.ext
let remove tenv x = List.filter (fun (x', _) -> not (x = x')) tenv
let defaultenv = emptytenv
let esubst : tysubst = []

let substitute tvar t tenv =
  List.map (fun (x, t') -> if t' = tvar then x, t else x, t') tenv
;;

let occurs tx t =
  let rec occurs tx t k =
    if tx = t
    then k true
    else (
      match t with
      | TArrow (t1, t2) ->
        occurs tx t1 (fun r1 ->
        occurs tx t2 (fun r2 ->
        k (r1 || r2))) [@ocamlformat "disable"]
      | _ -> k false)
  in
  occurs tx t (fun x -> x)
;;

let%test "occurs" = occurs TInt (TArrow (TInt, TBool))

let rec subst_ty subst t =
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TString -> TString
  | TUnit -> TUnit
  | TArrow (from_t, to_t) -> TArrow (subst_ty subst from_t, subst_ty subst to_t)
  | TVar x ->
    (match lookup x subst with
    | None -> TVar x
    | Some t -> t)
;;

let%test "subst_ty: simple" =
  let subst = emptytenv () in
  let subst = ext subst "x" TInt in
  subst_ty subst (TVar "x") = TInt
;;

let%test "subst_ty: complex" =
  let subst = emptytenv () in
  let subst = ext subst "x" TInt in
  let subst = ext subst "y" TBool in
  subst_ty subst (TArrow (TVar "x", TVar "y")) = TArrow (TInt, TBool)
;;

let subst_tyenv subst tenv = List.map (fun (x, t) -> x, subst_ty subst t) tenv

let subst_eql subst eql =
  List.map (fun (t1, t2) -> subst_ty subst t1, subst_ty subst t2) eql
;;

let compose_subst subst2 subst1 =
  let subst1' = List.map (fun (tx, t) -> tx, subst_ty subst2 t) subst1 in
  List.fold_left
    (fun subst (x, t) ->
      match lookup x subst1 with
      | Some _ -> subst
      | None -> (x, t) :: subst)
    subst1'
    subst2
;;

let unify eql =
  let rec solve eql subst =
    match eql with
    | [] -> Some subst
    | (t1, t2) :: eql ->
      if t1 = t2
      then solve eql subst
      else (
        match t1, t2 with
        | TArrow (from1, to1), TArrow (from2, to2) ->
          solve ((from1, from2) :: (to1, to2) :: eql) subst
        | TVar x, _ -> sub x t2 eql subst
        | _, TVar x -> sub x t1 eql subst
        | _, _ -> None)
  and sub x t eql subst =
    if occurs (TVar x) t
    then None
    else solve (subst_eql [ x, t ] eql) (compose_subst [ x, t ] subst)
  in
  match solve eql [] with
  | None -> failwith "unify: failed"
  | Some subst -> subst
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

let rec infer tenv e n =
  let ename = exp_name e in
  match e with
  | Var x ->
    (match lookup x tenv with
    | Some t -> tenv, t, esubst, n
    | None ->
      let tvar, n = new_typevar n in
      let tenv = ext tenv x tvar in
      tenv, tvar, esubst, n)
  | Unit -> tenv, TUnit, esubst, n
  | IntLit _ -> tenv, TInt, esubst, n
  | BoolLit _ -> tenv, TBool, esubst, n
  | StrLit _ -> tenv, TString, esubst, n
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
    binop_infer tenv e1 e2 n (fun (t1, t2) -> [ t1, TInt; t2, TInt ]) TInt
  | Greater (e1, e2) | Less (e1, e2) ->
    binop_infer tenv e1 e2 n (fun (t1, t2) -> [ t1, TInt; t2, TInt ]) TBool
  | Eq (e1, e2) -> binop_infer tenv e1 e2 n (fun (t1, t2) -> [ t1, t2 ]) TBool
  | If (c, et, ef) ->
    let tenv, ct, subst, n = infer tenv c n in
    let subst_c = unify [ ct, TBool ] in
    let subst = compose_subst subst subst_c in
    let tenv = subst_tyenv subst tenv in
    let tenv, tt, subst_t, n = infer tenv et n in
    let subst = compose_subst subst subst_t in
    let tenv = subst_tyenv subst tenv in
    let tenv, tf, subst_f, n = infer tenv ef n in
    let subst = compose_subst subst subst_f in
    let tt = subst_ty subst tt in
    let tf = subst_ty subst tf in
    let subst_r = unify [ tt, tf ] in
    let subst = compose_subst subst subst_r in
    let tt = subst_ty subst tt in
    tenv, tt, subst, n
  | Fun (x, e) ->
    let tvar, n = new_typevar n in
    let tenv = ext tenv x tvar in
    let tenv, t, subst, n = infer tenv e n in
    let tvar = subst_ty subst tvar in
    let tenv = remove tenv x in
    tenv, TArrow (tvar, t), subst, n
  | App (e1, e2) ->
    let tenv, t1, subst1, n = infer tenv e1 n in
    let tenv, t2, subst2, n = infer tenv e2 n in
    let tvar, n = new_typevar n in
    let t1 = subst_ty subst2 t1 in
    let subst3 = unify [ t1, TArrow (t2, tvar) ] in
    let tvar = subst_ty subst3 tvar in
    let tenv = subst_tyenv subst3 tenv in
    let subst4 = compose_subst subst1 subst2 in
    let subst4 = compose_subst subst3 subst4 in
    tenv, tvar, subst4, n
  | Let (x, e1, e2) ->
    let tenv, t1, subst1, n = infer tenv e1 n in
    let tenv = ext tenv x t1 in
    let tenv, t2, subst2, n = infer tenv e2 n in
    let subst = compose_subst subst1 subst2 in
    let t2 = subst_ty subst t2 in
    tenv, t2, subst, n
  | _ -> failwith @@ "infer: not implemented: " ^ ename

and binop_infer tenv e1 e2 n cmp retType =
  let tenv, t1, subst1, n = infer tenv e1 n in
  let tenv, t2, subst2, n = infer tenv e2 n in
  let t1 = subst_ty subst2 t1 in
  let subst3 = unify (cmp (t1, t2)) in
  let tenv = subst_tyenv subst3 tenv in
  let subst4 = compose_subst subst1 subst2 in
  let subst4 = compose_subst subst3 subst4 in
  tenv, retType, subst4, n
;;

let infer tenv e =
  let tenv, t, _, _ = infer tenv e 0 in
  tenv, t
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
