open Syntax

module Tyvar = struct
  type t = string * int

  let name (n, _) = n
  let age (_, a) = a
  let compare t1 t2 = compare (age t1) (age t2)
  let from_age n = n + 1, ("'a" ^ string_of_int n, n)
end

module TyvarSet = Set.Make (Tyvar)

type tyvar = Tyvar.t

type ty =
  | TInt
  | TBool
  | TString
  | TUnit
  | TArrow of ty * ty
  | TVar of tyvar
  | TList of ty

type scheme = TScheme of tyvar list * ty

let pprint_tyvar ppf t = Fmt.pf ppf "%s" (Tyvar.name t)
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
  | TVar x -> Fmt.pf ppf "@[<v 2>%s(%s)@]" tname (Tyvar.name x)
  | TList t -> Fmt.pf ppf "@[<v 2>%s@ %a@]" tname pprint_type t
;;

let pprint_scheme ppf ts =
  match ts with
  | TScheme ([], t) -> pprint_type ppf t
  | TScheme (tyvars, t) ->
    Fmt.pf
      ppf
      "@[<v 2>forall %a.@ %a@]"
      (Fmt.list ~sep:Fmt.comma pprint_tyvar)
      tyvars
      pprint_type
      t
;;

type tyenv = (string * scheme) list
type tysubst = (tyvar * ty) list

type ctx =
  { tenv : tyenv
  ; n : int
  }

let new_typevar ctx =
  let n, tvar = Tyvar.from_age ctx.n in
  { ctx with n = n + 1 }, TVar tvar
;;

let emptytenv () = []
let lookup = Eval.lookup
let ext = Eval.ext

let lookup_by_age tvar subst =
  let age = Tyvar.age tvar in
  List.find_map (fun (tvar, ty) -> if age = Tyvar.age tvar then Some ty else None) subst
;;

let remove x tenv = List.remove_assoc x tenv

let%test "remove" =
  let tenv = emptytenv () in
  let tenv = ext tenv "x" TInt in
  let tenv = ext tenv "y" TInt in
  let tenv' = ext tenv "x" TBool in
  remove "x" tenv' = tenv
;;

let defaultenv () =
  let _, ta = Tyvar.from_age (-1) in
  let _, tb = Tyvar.from_age (-2) in
  let _, tc = Tyvar.from_age (-3) in
  let tenv = emptytenv () in
  let tenv = ext tenv "failwith" (TScheme ([ ta ], TArrow (TString, TVar ta))) in
  let tenv = ext tenv "List.hd" (TScheme ([ tb ], TArrow (TList (TVar tb), TVar tb))) in
  let tenv =
    ext tenv "List.tl" (TScheme ([ tc ], TArrow (TList (TVar tc), TList (TVar tc))))
  in
  tenv
;;

let esubst : tysubst = []

let freevar e =
  let rec freevar e k =
    match e with
    | Var x -> k [ x ]
    | Cons (hd, tl) ->
      freevar hd @@ fun vars1 ->
      freevar tl @@ fun vars2 -> k @@ List.concat [ vars1; vars2 ]
    | Unit | IntLit _ | BoolLit _ | StrLit _ | _ -> k []
  in
  freevar e (fun x -> x)
;;

let%test "freevar" = freevar (Var "x") = [ "x" ]
let%test "freevar2" = freevar (Cons (Var "x", Cons (Var "y", Empty))) = [ "x"; "y" ]

let list_diff l1 l2 =
  let module SS = TyvarSet in
  let s1 = SS.of_list l1 in
  let s2 = SS.of_list l2 in
  let ret = SS.diff s1 s2 in
  SS.elements ret
;;

let list_inter l1 l2 =
  let module SS = TyvarSet in
  let s1 = SS.of_list l1 in
  let s2 = SS.of_list l2 in
  let ret = SS.inter s1 s2 in
  SS.elements ret
;;

let list_uniq l =
  let module SS = TyvarSet in
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
    match t with
    | TArrow (t1, t2) ->
      occurs tx t1 @@ fun r1 ->
      occurs tx t2 @@ fun r2 -> k (r1 || r2)
    | _ when t = tx -> k true
    | _ -> k false
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
    (match lookup_by_age x subst with
    | None -> TVar x
    | Some t -> t)
  | TList t -> TList (subst_ty subst t)
;;

let%test "subst_ty: simple" =
  let _, tx = Tyvar.from_age 0 in
  let subst = emptytenv () in
  let subst = ext subst tx TInt in
  subst_ty subst (TVar tx) = TInt
;;

let%test "subst_ty: complex" =
  let _, tx = Tyvar.from_age 0 in
  let _, ty = Tyvar.from_age 1 in
  let subst = emptytenv () in
  let subst = ext subst tx TInt in
  let subst = ext subst ty TBool in
  subst_ty subst (TArrow (TVar tx, TVar ty)) = TArrow (TInt, TBool)
;;

let subst_tvars (subst : tysubst) tvars =
  List.map
    (fun tvar ->
      match List.assoc_opt tvar subst with
      | Some (TVar y) -> y
      | _ -> tvar)
    tvars
;;

let%test "subst_tyvars" =
  let _, tx = Tyvar.from_age 0 in
  let _, ty = Tyvar.from_age 1 in
  let _, tz = Tyvar.from_age 2 in
  subst_tvars [ tx, TVar tz ] [ tx; ty ] = [ tz; ty ]
;;

let vars_of_subst (subst : tysubst) =
  list_uniq @@ List.flatten @@ List.map (fun (x, t) -> x :: freetyvar_ty t) subst
;;

let subst_ts subst ts ctx =
  match ts with
  | TScheme (tvars, t) ->
    let collisionvars = list_inter tvars @@ vars_of_subst subst in
    let ctx, subst' =
      List.fold_left
        (fun (ctx, subst') var ->
          let ctx, newvar = new_typevar ctx in
          let subst' = (var, newvar) :: subst' in
          ctx, subst')
        (ctx, [])
        collisionvars
    in
    (* fresh *)
    let tvars = subst_tvars subst' tvars in
    let t = subst_ty subst' t in
    let tvars = subst_tvars subst tvars in
    let t = subst_ty subst t in
    ctx, TScheme (tvars, t)
;;

let subst_tyenv subst ctx =
  List.fold_right
    (fun (x, ts) ctx ->
      let ctx, ts = subst_ts subst ts ctx in
      let tenv = ext ctx.tenv x ts in
      { ctx with tenv })
    ctx.tenv
    { ctx with tenv = defaultenv () }
;;

let subst_eql subst eql =
  List.map (fun (t1, t2) -> subst_ty subst t1, subst_ty subst t2) eql
;;

let compose_subst subst2 subst1 =
  let subst1' = List.map (fun (tx, t) -> tx, subst_ty subst2 t) subst1 in
  List.fold_left
    (fun subst (x, t) ->
      match lookup_by_age x subst1 with
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
    else (
      match solve (subst_eql [ x, t ] eql) [] with
      | Ok subst' ->
        let subst = compose_subst subst subst' in
        let subst = compose_subst subst [ x, t ] in
        Ok subst
      | _ as e -> e)
  in
  match solve eql [] with
  | Error message -> failwith @@ "unify failed: " ^ message
  | Ok subst -> subst
;;

let%test "unify_1" =
  let n = 0 in
  let n, t0 = Tyvar.from_age n in
  let n, t1 = Tyvar.from_age n in
  let _, t2 = Tyvar.from_age n in
  let subst = unify [ TVar t0, TVar t1; TVar t0, TVar t2 ] in
  [ t1, TVar t2; t0, TVar t2 ] = subst
;;

let%test "unify_2" =
  let n = 0 in
  let n, t0 = Tyvar.from_age n in
  let n, t1 = Tyvar.from_age n in
  let _, t2 = Tyvar.from_age n in
  let subst = unify [ TVar t0, TVar t1; TVar t1, TVar t2 ] in
  [ t1, TVar t2; t0, TVar t2 ] = subst
;;

let compose_subst_with_infer subst1 subst2 =
  let subst = compose_subst subst1 subst2 in
  let eql = List.map (fun (tv, t) -> TVar tv, t) subst in
  let subst = unify eql in
  subst
;;

let instantiate ts ctx =
  match ts with
  | TScheme (tvars, t) ->
    let ctx, subst =
      List.fold_left
        (fun (ctx, subst) tvar ->
          let ctx, newtvar = new_typevar ctx in
          let subst = compose_subst subst [ tvar, newtvar ] in
          ctx, subst)
        (ctx, [])
        tvars
    in
    let t = subst_ty subst t in
    ctx, t
;;

let generalize t tenv =
  let tvars = list_diff (freetyvar_ty t) (freetyvar_tyenv tenv) in
  TScheme (tvars, t)
;;

let%test "generalize: simple" = generalize TInt (emptytenv ()) = TScheme ([], TInt)

let%test "generalize: complex" =
  let ta = "a", 0 in
  let tb = "b", 1 in
  let tenv = emptytenv () in
  let tenv = ext tenv "xxxxx" (TScheme ([], TVar ta)) in
  let t = TArrow (TVar ta, TVar tb) in
  generalize t tenv = TScheme ([ tb ], t)
;;

let compose_subst = compose_subst_with_infer

let rec infer ctx e =
  match e with
  | Var x ->
    (match lookup x ctx.tenv with
    | Some ts ->
      let ctx, t = instantiate ts ctx in
      ctx, t, esubst
    | None -> failwith @@ "failed to lookup type of var " ^ x)
  | Unit -> ctx, TUnit, esubst
  | IntLit _ -> ctx, TInt, esubst
  | BoolLit _ -> ctx, TBool, esubst
  | StrLit _ -> ctx, TString, esubst
  | FailWith _ ->
    let ctx, tvar = new_typevar ctx in
    ctx, tvar, esubst
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
    binop_infer ctx e1 e2 (fun (t1, t2) -> [ t1, TInt; t2, TInt ]) TInt
  | Greater (e1, e2) | Less (e1, e2) ->
    binop_infer ctx e1 e2 (fun (t1, t2) -> [ t1, TInt; t2, TInt ]) TBool
  | Eq (e1, e2) -> binop_infer ctx e1 e2 (fun (t1, t2) -> [ t1, t2 ]) TBool
  | If (c, et, ef) ->
    let ctx, ct, subst = infer ctx c in
    let subst_c = unify [ ct, TBool ] in
    let subst = compose_subst subst subst_c in
    let ctx = subst_tyenv subst ctx in
    let ctx, tt, subst_t = infer ctx et in
    let subst = compose_subst subst subst_t in
    let ctx = subst_tyenv subst ctx in
    let ctx, tf, subst_f = infer ctx ef in
    let subst = compose_subst subst subst_f in
    let tt = subst_ty subst tt in
    let tf = subst_ty subst tf in
    let subst_r = unify [ tt, tf ] in
    let subst = compose_subst subst subst_r in
    let tt = subst_ty subst tt in
    ctx, tt, subst
  | Fun (x, e) ->
    let ctx, tvar = new_typevar ctx in
    let tenv = ext ctx.tenv x (ty_of_scheme tvar) in
    let ctx, t, subst = infer { ctx with tenv } e in
    let tvar = subst_ty subst tvar in
    let tenv = remove x ctx.tenv in
    { ctx with tenv }, TArrow (tvar, t), subst
  | App (e1, e2) ->
    let ctx, t1, subst1 = infer ctx e1 in
    let ctx, t2, subst2 = infer ctx e2 in
    let ctx, tvar = new_typevar ctx in
    let t1 = subst_ty subst2 t1 in
    let subst3 = unify [ t1, TArrow (t2, tvar) ] in
    let tvar = subst_ty subst3 tvar in
    let ctx = subst_tyenv subst3 ctx in
    let subst4 = compose_subst subst1 subst2 in
    let subst4 = compose_subst subst3 subst4 in
    ctx, tvar, subst4
  | Let (x, e1, e2) ->
    let ctx, t1, subst1 = infer ctx e1 in
    let tenv = ctx.tenv in
    let t1 = generalize t1 tenv in
    let tenv = ext tenv x t1 in
    let ctx, t2, subst2 = infer { ctx with tenv } e2 in
    let subst = compose_subst subst1 subst2 in
    let t2 = subst_ty subst t2 in
    let tenv = ctx.tenv in
    let tenv = remove x tenv in
    { ctx with tenv }, t2, subst
  | LetRec (f, x, e1, e2) ->
    let ctx, tvar_fn = new_typevar ctx in
    let ctx, tvar_arg = new_typevar ctx in
    let tenv = ctx.tenv in
    let tenv = ext tenv f (ty_of_scheme tvar_fn) in
    let tenv = ext tenv x (ty_of_scheme tvar_arg) in
    let ctx, t1, subst = infer { ctx with tenv } e1 in
    let tvar_fn = subst_ty subst tvar_fn in
    let tvar_arg = subst_ty subst tvar_arg in
    let subst' = unify [ tvar_fn, TArrow (tvar_arg, t1) ] in
    let subst = compose_subst subst subst' in
    let ctx = subst_tyenv subst ctx in
    let tenv = ctx.tenv in
    let tvar_fn = subst_ty subst tvar_fn in
    let tenv = remove f tenv in
    let tenv = remove x tenv in
    let tvar_fn = generalize tvar_fn tenv in
    let tenv = ext tenv f tvar_fn in
    let ctx, t2, subst' = infer { ctx with tenv } e2 in
    let subst = compose_subst subst subst' in
    let t2 = subst_ty subst t2 in
    ctx, t2, subst
  | Match (e1, cases) ->
    let loop (subst, ctx, bt, t1) (p, b) =
      let vars = freevar p in
      let ctx =
        List.fold_left
          (fun ctx var ->
            let ctx, tvar = new_typevar ctx in
            let tenv = ext ctx.tenv var (ty_of_scheme tvar) in
            { ctx with tenv })
          ctx
          vars
      in
      let ctx, pt, subst' = infer ctx p in
      let subst = compose_subst subst subst' in
      let subst' = unify [ t1, pt ] in
      let subst = compose_subst subst subst' in
      let ctx = subst_tyenv subst ctx in
      let ctx, bt', subst' = infer ctx b in
      let subst = compose_subst subst subst' in
      let subst' = unify [ bt, bt' ] in
      let subst = compose_subst subst subst' in
      let t1 = subst_ty subst t1 in
      let bt = subst_ty subst bt in
      let tenv = List.fold_left (fun tenv var -> remove var tenv) ctx.tenv vars in
      subst, { ctx with tenv }, bt, t1
    in
    let ctx, t1, subst = infer ctx e1 in
    let ctx, bt = new_typevar ctx in
    let subst, ctx, bt, _ = List.fold_left loop (subst, ctx, bt, t1) cases in
    let ctx = subst_tyenv subst ctx in
    ctx, bt, subst
  | Empty ->
    let ctx, tvar = new_typevar ctx in
    ctx, TList tvar, esubst
  | Cons (e1, e2) ->
    let ctx, t1, subst1 = infer ctx e1 in
    let ctx, t2, subst2 = infer ctx e2 in
    let t1 = subst_ty subst2 t1 in
    let subst = compose_subst subst1 subst2 in
    let subst' = unify [ t2, TList t1 ] in
    let subst = compose_subst subst subst' in
    let t2 = subst_ty subst t2 in
    let ctx = subst_tyenv subst ctx in
    ctx, t2, subst

and binop_infer ctx e1 e2 cmp retType =
  let ctx, t1, subst1 = infer ctx e1 in
  let ctx, t2, subst2 = infer ctx e2 in
  let t1 = subst_ty subst2 t1 in
  let subst3 = unify (cmp (t1, t2)) in
  let ctx = subst_tyenv subst3 ctx in
  let subst4 = compose_subst subst1 subst2 in
  let subst4 = compose_subst subst3 subst4 in
  ctx, retType, subst4
;;

let infer tenv e =
  let ctx, t, _ = infer { tenv; n = 0 } e in
  ctx.tenv, t
;;
