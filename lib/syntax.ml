type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | Fun of string * exp
  | App of exp * exp
  | Let of string * exp * exp
  | LetRec of string * string * exp * exp
  | If of exp * exp * exp
  | Match of exp * (exp * exp) list
  | Eq of exp * exp
  | Greater of exp * exp
  | Less of exp * exp
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Empty (* [] *)
  | Unit (* () *)
  | Cons of exp * exp
  | FailWith of exp

type value =
  | UnitVal
  | IntVal of int
  | BoolVal of bool
  | StrVal of string
  | ListVal of value list
  | FunVal of string * exp * env
  | RecFunVal of string * string * exp * env

and env = (string * value) list

let exp_name = function
  | Var _ -> "Var"
  | IntLit _ -> "IntLit"
  | BoolLit _ -> "BoolLit"
  | StrLit _ -> "StrLit"
  | Fun _ -> "Fun"
  | App _ -> "App"
  | Let _ -> "Let"
  | LetRec _ -> "LetRec"
  | If _ -> "If"
  | Match _ -> "Match"
  | Eq _ -> "Eq"
  | Greater _ -> "Greater"
  | Less _ -> "Less"
  | Plus _ -> "Plus"
  | Minus _ -> "Minus"
  | Times _ -> "Times"
  | Div _ -> "Div"
  | Empty -> "Empty"
  | Unit -> "Unit"
  | Cons _ -> "Cons"
  | FailWith _ -> "FailWith"
;;

let value_type = function
  | UnitVal -> "unit"
  | IntVal _ -> "int"
  | BoolVal _ -> "bool"
  | StrVal _ -> "str"
  | ListVal _ -> "list"
  | FunVal _ -> "fun"
  | RecFunVal _ -> "fun"
;;

let rec pprint_exp ppf e =
  let ename = exp_name e in
  match e with
  | Empty | Unit -> Fmt.pf ppf "%s" ename
  | Var x -> Fmt.pf ppf "%s %s" ename x
  | IntLit n -> Fmt.pf ppf "%s %d" ename n
  | BoolLit b -> Fmt.pf ppf "%s %b" ename b
  | StrLit s -> Fmt.pf ppf "%s %s" ename s
  | FailWith e -> Fmt.pf ppf "@[<v 2>%s@ %a]" ename pprint_exp e
  | App (e1, e2)
  | Eq (e1, e2)
  | Greater (e1, e2)
  | Less (e1, e2)
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Div (e1, e2)
  | Cons (e1, e2) -> Fmt.pf ppf "@[<v 2>%s@ %a@ %a@]" ename pprint_exp e1 pprint_exp e2
  | If (ce, e1, e2) ->
    Fmt.pf ppf "@[<v 2>%s@ %a@ %a@ %a@]" ename pprint_exp ce pprint_exp e1 pprint_exp e2
  | Fun (n, e) -> Fmt.pf ppf "@[<v 2>%s@ %s@ %a@]" ename n pprint_exp e
  | Let (n, e1, e2) ->
    Fmt.pf ppf "@[<v 2>%s@ %s@ %a@ %a@]" ename n pprint_exp e1 pprint_exp e2
  | LetRec (n, x, e1, e2) ->
    Fmt.pf ppf "@[<v 2>%s@ %s@ %s@ %a@ %a@]" ename n x pprint_exp e1 pprint_exp e2
  | Match (e, es) ->
    let pprint_pattern ppf (e1, e2) =
      Fmt.pf ppf "@[<v 2>%a@]" (Fmt.pair ~sep:Fmt.semi pprint_exp pprint_exp) (e1, e2)
    in
    Fmt.pf ppf "@[<v 2>%s@ %a@ %a@]" ename pprint_exp e (Fmt.list pprint_pattern) es
;;

let rec pprint_value ppf = function
  | UnitVal -> Fmt.pf ppf "UnitVal"
  | IntVal n -> Fmt.pf ppf "IntVal %d" n
  | BoolVal b -> Fmt.pf ppf "BoolVal %b" b
  | StrVal s -> Fmt.pf ppf "BoolVal %s" s
  | ListVal l -> Fmt.pf ppf "@[<v 2>ListVal@ %a@]" (Fmt.list pprint_value) l
  | FunVal (n, e, env) ->
    Fmt.pf ppf "@[<v 2>FunVal@ %s@ %a@ %a@]" n pprint_exp e pprint_env env
  | RecFunVal (n, x, e, env) ->
    Fmt.pf ppf "@[<v 2>RetFunVal@ %s@ %s@ %a@ %a@]" n x pprint_exp e pprint_env env

and pprint_env ppf env =
  let pp_list = Fmt.(list (pair string pprint_value)) in
  Fmt.pf ppf "@[<v 2>env@ %a@]" pp_list env
;;
