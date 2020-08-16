open Syntax

type tyvar = string

type ty =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of ty * ty
  | TVar of tyvar
  | TList of ty
  | TScheme of tyvar list * ty

let type_name = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow _ -> "fun"
  | TVar _ -> "tvar"
  | TList _ -> "list"
  | TScheme _ -> "type schema"
;;

let rec pprint_type ppf t =
  let tname = type_name t in
  match t with
  | TInt | TBool | TString | TUnit -> Fmt.pf ppf "%s" tname
  | TArrow (t1, t2) ->
    Fmt.pf ppf "@[<v 2>%s@ %a->@ %a@]" tname pprint_type t1 pprint_type t2
  | TVar x -> Fmt.pf ppf "@[<v 2>%s(%s)@]" tname x
  | TList t -> Fmt.pf ppf "@[<v 2>%s@ %a@]" tname pprint_type t
  | TScheme (tyvars, t) ->
    Fmt.pf
      ppf
      "@[<v 2>%s: forall %a.@ %a@]"
      tname
      (Fmt.list Fmt.string)
      tyvars
      pprint_type
      t
;;

let new_typevar n = TVar ("'a" ^ string_of_int n), n + 1

type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let emptytenv () : tyenv = []
let lookup = Eval.lookup
let ext = Eval.ext

let remove tenv x =
  let rec remove tenv x k =
    match tenv with
    | [] -> k []
    | (tx, _) :: tenv when tx = x -> k tenv
    | h :: tenv -> remove tenv x (fun tenv -> k (h :: tenv))
  in
  remove tenv x (fun x -> x)
;;

let%test "remove" =
  let tenv = emptytenv () in
  let tenv = ext tenv "x" TInt in
  let tenv' = ext tenv "y" TInt in
  let tenv = ext tenv' "x" TBool in
  remove tenv "x" = tenv'
;;

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
  | TList t -> TList (subst_ty subst t)
  | TScheme _ as t -> t
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
    | [] -> Ok subst
    | (t1, t2) :: eql ->
      if t1 = t2
      then solve eql subst
      else (
        match t1, t2 with
        | TArrow (from1, to1), TArrow (from2, to2) ->
          solve ((from1, from2) :: (to1, to2) :: eql) subst
        | TList t1, TList t2 -> solve ((t1, t2) :: eql) subst
        | TVar x, _ -> sub x t2 eql subst
        | _, TVar x -> sub x t1 eql subst
        | _, _ ->
          Error
            (Fmt.strf
               "@[expected %s but got %s:@ One: %a@ Another: %a@]"
               (type_name t1)
               (type_name t2)
               pprint_type
               t1
               pprint_type
               t2))
  and sub x t eql subst =
    if occurs (TVar x) t
    then Error (Fmt.strf "type %s contains a reference to itself" (type_name t))
    else solve (subst_eql [ x, t ] eql) (compose_subst [ x, t ] subst)
  in
  match solve eql [] with
  | Error message -> failwith @@ "unify failed: " ^ message
  | Ok subst -> subst
;;

let freevar e =
  let rec freevar e k =
    match e with
    | Var x ->
      (match x with
      | "_" -> k []
      | x -> k [ x ])
    | Cons (hd, tl) ->
      freevar hd (fun vars1 ->
          freevar tl (fun vars2 -> k @@ List.concat [ vars1; vars2 ]))
    | Unit | IntLit _ | BoolLit _ | StrLit _ | _ -> k []
  in
  freevar e (fun x -> x)
;;

let%test "freevar" = freevar (Var "x") = [ "x" ]
let%test "freevar2" = freevar (Cons (Var "x", Cons (Var "y", Empty))) = [ "x"; "y" ]

let list_diff l1 l2 =
  let module SS = Set.Make (String) in
  let s1 = SS.of_list l1 in
  let s2 = SS.of_list l2 in
  let ret = SS.diff s1 s2 in
  SS.elements ret
;;

let rec freetyvar_ty t =
  match t with
  | TInt | TBool | TString | TUnit -> []
  | TArrow (t1, t2) -> List.concat @@ List.map freetyvar_ty [ t1; t2 ]
  | TVar x -> [ x ]
  | TList t -> freetyvar_ty t
  | TScheme (tyvars, t) -> list_diff (freetyvar_ty t) tyvars
;;

let rec freetyvar_tyenv tenv =
  match tenv with
  | [] -> []
  | (_, t) :: tenv -> List.concat @@ [ freetyvar_ty t; freetyvar_tyenv tenv ]
;;

let closure t tenv =
  let tvars = list_diff (freetyvar_ty t) (freetyvar_tyenv tenv) in
  TScheme (tvars, t)
;;

let rec infer tenv e n =
  match e with
  | Var x ->
    (match lookup x tenv with
    | Some (TScheme (tvars, t)) ->
      let n, subst =
        List.fold_left
          (fun (n, subst) tvar ->
            let newtvar, n = new_typevar n in
            let subst = compose_subst subst [ tvar, newtvar ] in
            n, subst)
          (n, esubst)
          tvars
      in
      tenv, t, subst, n
    | Some t -> tenv, t, esubst, n
    | None ->
      let tvar, n = new_typevar n in
      let tenv = ext tenv x tvar in
      tenv, tvar, esubst, n)
  | Unit -> tenv, TUnit, esubst, n
  | IntLit _ -> tenv, TInt, esubst, n
  | BoolLit _ -> tenv, TBool, esubst, n
  | StrLit _ -> tenv, TString, esubst, n
  | FailWith _ ->
    let tvar, n = new_typevar n in
    tenv, tvar, esubst, n
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
    let t1 = closure t1 tenv in
    let tenv = ext tenv x t1 in
    let tenv, t2, subst2, n = infer tenv e2 n in
    let subst = compose_subst subst1 subst2 in
    let t2 = subst_ty subst t2 in
    tenv, t2, subst, n
  | LetRec (f, x, e1, e2) ->
    let tvar_a, n = new_typevar n in
    let tvar_r, n = new_typevar n in
    let tenv = ext tenv x tvar_a in
    let tenv = ext tenv f @@ TArrow (tvar_a, tvar_r) in
    let tenv, t1, subst, n = infer tenv e1 n in
    let tvar_r = subst_ty subst tvar_r in
    let subst' = unify [ t1, tvar_r ] in
    let subst = compose_subst subst subst' in
    let tenv = subst_tyenv subst tenv in
    let tenv, t2, subst', n = infer tenv e2 n in
    let subst = compose_subst subst subst' in
    let t2 = subst_ty subst t2 in
    tenv, t2, subst, n
  | Match (e1, cases) ->
    let loop (subst1, tenv, n, bt, t1) (p, b) =
      let vars = freevar p in
      let tenv, n =
        List.fold_left
          (fun (tenv, n) var ->
            let tvar, n = new_typevar n in
            let tenv = ext tenv var tvar in
            tenv, n)
          (tenv, n)
          vars
      in
      let tenv, pt, subst', n = infer tenv p n in
      let subst = compose_subst subst1 subst' in
      let subst' = unify [ t1, pt ] in
      let subst = compose_subst subst subst' in
      let tenv = subst_tyenv subst tenv in
      let tenv, bt', subst_b, n = infer tenv b n in
      let subst = compose_subst subst subst_b in
      let subst' = unify [ bt, bt' ] in
      let subst_b = compose_subst subst_b subst' in
      let subst = compose_subst subst subst' in
      let bt = subst_ty subst bt in
      let tenv = List.fold_left (fun tenv var -> remove tenv var) tenv vars in
      let subst = compose_subst subst1 subst_b in
      subst, tenv, n, bt, t1
    in
    let tenv, t1, subst1, n = infer tenv e1 n in
    let bt, n = new_typevar n in
    let subst, tenv, n, bt, _ = List.fold_left loop (subst1, tenv, n, bt, t1) cases in
    let tenv = subst_tyenv subst tenv in
    tenv, bt, subst, n
  | Empty ->
    let tvar, n = new_typevar n in
    tenv, TList tvar, esubst, n
  | Cons (e1, e2) ->
    let tenv, t1, subst1, n = infer tenv e1 n in
    let tenv, t2, subst2, n = infer tenv e2 n in
    let t1 = subst_ty subst2 t1 in
    let subst = compose_subst subst1 subst2 in
    let subst' = unify [ t2, TList t1 ] in
    let subst = compose_subst subst subst' in
    let t2 = subst_ty subst t2 in
    let tenv = subst_tyenv subst tenv in
    tenv, t2, subst, n

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
  | FailWith _ -> TUnit
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
  | LetRec _ -> failwith "letrec: not implemented"
  | Match _ -> failwith "match: not implemented"
  | Cons _ -> failwith "cons: not implemented"
  | Empty -> failwith "empty: not implemented"

and binop ename e1 e2 retType tenv =
  match check e1 tenv, check e2 tenv with
  | TInt, TInt -> retType
  | _ -> failwith @@ "type error: " ^ ename
;;
