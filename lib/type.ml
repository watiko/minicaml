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

type scheme = TScheme of tyvar list * ty

let ty_of_scheme t = TScheme ([], t)

let type_name = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TArrow _ -> "fun"
  | TVar _ -> "tvar"
  | TList _ -> "list"
;;

let rec pprint_type ppf t =
  let tname = type_name t in
  match t with
  | TInt | TBool | TString | TUnit -> Fmt.pf ppf "%s" tname
  | TArrow (t1, t2) ->
    Fmt.pf ppf "@[<v 2>%s@ %a->@ %a@]" tname pprint_type t1 pprint_type t2
  | TVar x -> Fmt.pf ppf "@[<v 2>%s(%s)@]" tname x
  | TList t -> Fmt.pf ppf "@[<v 2>%s@ %a@]" tname pprint_type t
;;

let pprint_schema ppf ts =
  match ts with
  | TScheme ([], t) -> pprint_type ppf t
  | TScheme (tyvars, t) ->
    Fmt.pf
      ppf
      "@[<v 2>forall %a.@ %a@]"
      (Fmt.list ~sep:Fmt.comma Fmt.string)
      tyvars
      pprint_type
      t
;;

let new_typevar n = TVar ("'a" ^ string_of_int n), n + 1

type tyenv = (string * scheme) list
type tysubst = (tyvar * ty) list

let emptytenv () = []
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

let defaultenv () =
  let ta = TVar "'a" in
  let lista = TList ta in
  let tenv = emptytenv () in
  let tenv = ext tenv "failwith" (TScheme ([ "'a" ], TArrow (TString, ta))) in
  let tenv = ext tenv "List.hd" (TScheme ([ "'a" ], TArrow (lista, ta))) in
  let tenv = ext tenv "List.tl" (TScheme ([ "'a" ], TArrow (lista, lista))) in
  tenv
;;

let esubst : tysubst = []

let freevar e =
  let rec freevar e k =
    match e with
    | Var x -> k [ x ]
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

let list_inter l1 l2 =
  let module SS = Set.Make (String) in
  let s1 = SS.of_list l1 in
  let s2 = SS.of_list l2 in
  let ret = SS.inter s1 s2 in
  SS.elements ret
;;

let list_uniq l =
  let module SS = Set.Make (String) in
  let s = SS.of_list l in
  SS.elements s
;;

let rec freetyvar_ty t =
  match t with
  | TInt | TBool | TString | TUnit -> []
  | TArrow (t1, t2) -> List.concat [ freetyvar_ty t1; freetyvar_ty t2 ]
  | TVar x -> [ x ]
  | TList t -> freetyvar_ty t
;;

let freetyvar_sc ts =
  match ts with
  | TScheme (tyvars, t) -> list_diff (freetyvar_ty t) tyvars
;;

let rec freetyvar_tyenv tenv =
  match tenv with
  | [] -> []
  | (_, t) :: tenv -> List.concat [ freetyvar_sc t; freetyvar_tyenv tenv ]
;;

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

let subst_tvars (subst : tysubst) tvars =
  List.map
    (fun tvar ->
      let substElementOpt = List.find_opt (fun (x, _) -> tvar = x) subst in
      match substElementOpt with
      | None -> tvar
      | Some (_, t) ->
        (match t with
        | TVar y -> y
        | _ -> tvar))
    tvars
;;

let%test "subst_tyvars" = subst_tvars [ "x", TVar "1" ] [ "x"; "y" ] = [ "1"; "y" ]

let vars_of_subst (subst : tysubst) =
  list_uniq @@ List.flatten @@ List.map (fun (x, t) -> x :: freetyvar_ty t) subst
;;

let subst_ts subst ts n =
  match ts with
  | TScheme (tvars, t) ->
    let collisionvars = list_inter tvars @@ vars_of_subst subst in
    let subst', n =
      List.fold_left
        (fun (subst', n) var ->
          let newvar, n = new_typevar n in
          let subst' = (var, newvar) :: subst' in
          subst', n)
        ([], n)
        collisionvars
    in
    (* fresh *)
    let tvars = subst_tvars subst' tvars in
    let t = subst_ty subst' t in
    let tvars = subst_tvars subst tvars in
    let t = subst_ty subst t in
    TScheme (tvars, t), n
;;

let subst_tyenv subst tenv n =
  List.fold_right
    (fun (x, ts) (tenv, n) ->
      let ts, n = subst_ts subst ts n in
      let tenv = ext tenv x ts in
      tenv, n)
    tenv
    (emptytenv (), n)
;;

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

let instantiate ts n =
  match ts with
  | TScheme (tvars, t) ->
    let subst, n =
      List.fold_left
        (fun (subst, n) tvar ->
          let newtvar, n = new_typevar n in
          let subst = compose_subst subst [ tvar, newtvar ] in
          subst, n)
        ([], n)
        tvars
    in
    let t = subst_ty subst t in
    t, n
;;

let generalize t tenv =
  let tvars = list_diff (freetyvar_ty t) (freetyvar_tyenv tenv) in
  TScheme (tvars, t)
;;

let%test "generalize: simple" = generalize TInt (emptytenv ()) = TScheme ([], TInt)

let%test "generalize: complex" =
  let tenv = emptytenv () in
  let tenv = ext tenv "xxxxx" (TScheme ([], TVar "a")) in
  let t = TArrow (TVar "a", TVar "b") in
  generalize t tenv = TScheme ([ "b" ], t)
;;

let rec infer tenv e n =
  match e with
  | Var x ->
    (match lookup x tenv with
    | Some ts ->
      let t, n = instantiate ts n in
      tenv, t, esubst, n
    | None -> failwith @@ "failed to lookup type of var " ^ x)
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
    let tenv, n = subst_tyenv subst tenv n in
    let tenv, tt, subst_t, n = infer tenv et n in
    let subst = compose_subst subst subst_t in
    let tenv, n = subst_tyenv subst tenv n in
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
    let tenv = ext tenv x (ty_of_scheme tvar) in
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
    let tenv, n = subst_tyenv subst3 tenv n in
    let subst4 = compose_subst subst1 subst2 in
    let subst4 = compose_subst subst3 subst4 in
    tenv, tvar, subst4, n
  | Let (x, e1, e2) ->
    let tenv, t1, subst1, n = infer tenv e1 n in
    let t1 = generalize t1 tenv in
    let tenv = ext tenv x t1 in
    let tenv, t2, subst2, n = infer tenv e2 n in
    let subst = compose_subst subst1 subst2 in
    let t2 = subst_ty subst t2 in
    let tenv = remove tenv x in
    tenv, t2, subst, n
  | LetRec (f, x, e1, e2) ->
    let tvar_fn, n = new_typevar n in
    let tvar_arg, n = new_typevar n in
    let tenv = ext tenv f (ty_of_scheme tvar_fn) in
    let tenv = ext tenv x (ty_of_scheme tvar_arg) in
    let tenv, t1, subst, n = infer tenv e1 n in
    (* todo: simplify subst *)
    let tvar_fn = subst_ty subst tvar_fn in
    let tvar_fn = subst_ty subst tvar_fn in
    let tvar_arg = subst_ty subst tvar_arg in
    let subst' = unify [ tvar_fn, TArrow (tvar_arg, t1) ] in
    let subst = compose_subst subst subst' in
    let tenv, n = subst_tyenv subst tenv n in
    let tvar_fn = subst_ty subst tvar_fn in
    let tvar_fn = generalize tvar_fn tenv in
    let tenv = remove tenv f in
    let tenv = ext tenv f tvar_fn in
    let tenv = remove tenv x in
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
            let tenv = ext tenv var (ty_of_scheme tvar) in
            tenv, n)
          (tenv, n)
          vars
      in
      let tenv, pt, subst', n = infer tenv p n in
      let subst = compose_subst subst1 subst' in
      let subst' = unify [ t1, pt ] in
      let subst = compose_subst subst subst' in
      let tenv, n = subst_tyenv subst tenv n in
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
    let tenv, n = subst_tyenv subst tenv n in
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
    let tenv, n = subst_tyenv subst tenv n in
    tenv, t2, subst, n

and binop_infer tenv e1 e2 n cmp retType =
  let tenv, t1, subst1, n = infer tenv e1 n in
  let tenv, t2, subst2, n = infer tenv e2 n in
  let t1 = subst_ty subst2 t1 in
  let subst3 = unify (cmp (t1, t2)) in
  let tenv, n = subst_tyenv subst3 tenv n in
  let subst4 = compose_subst subst1 subst2 in
  let subst4 = compose_subst subst3 subst4 in
  tenv, retType, subst4, n
;;

let infer tenv e =
  let tenv, t, _, _ = infer tenv e 0 in
  tenv, t
;;
