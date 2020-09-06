open Syntax

type env = Syntax.env

let unsafeParse s =
  let result = Parser.(parse main (explode s)) in
  match result with
  | Some exp -> exp
  | None -> failwith @@ "parse error: " ^ s
;;

let emptyenv () : env = []
let ext env x v = (x, v) :: env

let defaultenv () =
  let hdExp =
    unsafeParse {|
    match l with
    | h :: _ -> h
    | _ -> failwith "hd"
  |}
  in
  let tlExp =
    unsafeParse {|
    match l with
    | _ :: tl -> tl
    | _ -> failwith "tl"
  |}
  in
  let env = emptyenv () in
  let env = ext env "failwith" (FunVal ("s", FailWith (Var "s"), env)) in
  let env = ext env "List.hd" (FunVal ("l", hdExp, env)) in
  let env = ext env "List.tl" (FunVal ("l", tlExp, env)) in
  env
;;

let lookup x env = List.assoc_opt x env

let rec eval e env k =
  let binop f e1 e2 env k =
    eval e1 env @@ fun v1 ->
    eval e2 env @@ fun v2 ->
    match v1, v2 with
    | IntVal n1, IntVal n2 -> k @@ IntVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  let condop f e1 e2 env k =
    eval e1 env @@ fun v1 ->
    eval e2 env @@ fun v2 ->
    match v1, v2 with
    | IntVal n1, IntVal n2 -> k @@ BoolVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | Var x ->
    (match lookup x env with
    | Some v -> k v
    | None -> failwith @@ "lookup failed with key: " ^ x)
  | IntLit n -> k @@ IntVal n
  | BoolLit b -> k @@ BoolVal b
  | StrLit s -> k @@ StrVal s
  | Fun (x, e1) -> k @@ FunVal (x, e1, env)
  | App (e1, e2) -> app_eval e1 e2 env k
  | Let (x, e1, e2) ->
    eval e1 env @@ fun v1 ->
    let env = ext env x v1 in
    eval e2 env k
  | LetRec (f, x, e1, e2) ->
    let env = ext env f (RecFunVal (f, x, e1, env)) in
    eval e2 env k
  | Plus (e1, e2) -> binop ( + ) e1 e2 env k
  | Minus (e1, e2) -> binop ( - ) e1 e2 env k
  | Times (e1, e2) -> binop ( * ) e1 e2 env k
  | Div (e1, e2) -> binop ( / ) e1 e2 env k
  | Greater (e1, e2) -> condop ( > ) e1 e2 env k
  | Less (e1, e2) -> condop ( < ) e1 e2 env k
  | Eq (e1, e2) -> eq_eval e1 e2 env k
  | If (c, e1, e2) ->
    eval c env @@ fun v1 ->
    (match v1 with
    | BoolVal true -> eval e1 env k
    | BoolVal false -> eval e2 env k
    | v -> failwith @@ "if: cond type is not bool but got: " ^ value_type v)
  | Unit -> k UnitVal
  | Empty -> k @@ ListVal []
  | Cons (e1, e2) ->
    eval e1 env @@ fun v1 ->
    eval e2 env @@ fun v2 ->
    (match v1, v2 with
    | v1, ListVal v2 -> k @@ ListVal (v1 :: v2)
    | v1, v2 ->
      failwith
      @@ "cons: required list type but got: "
      ^ value_type v1
      ^ ", "
      ^ value_type v2)
  | FailWith e ->
    eval e env @@ fun v1 ->
    (match v1 with
    | StrVal s -> failwith s
    | v -> failwith @@ "failwith: required str type but got: " ^ value_type v)
  | Match (e1, cases) -> match_eval e1 cases env k

and app_eval e1 e2 env k =
  eval e1 env @@ fun fn ->
  eval e2 env @@ fun arg ->
  match fn with
  | FunVal (x, body, fenv) ->
    let fenv = ext fenv x arg in
    eval body fenv k
  | RecFunVal (f, x, body, fenv) ->
    let fenv = ext fenv x arg in
    let fenv = ext fenv f fn in
    eval body fenv k
  | _ -> failwith "app: function value required"

and eq_eval e1 e2 env k =
  eval e1 env @@ fun v1 ->
  eval e2 env @@ fun v2 ->
  match v1, v2 with
  | IntVal n1, IntVal n2 -> k @@ BoolVal (n1 = n2)
  | BoolVal b1, BoolVal b2 -> k @@ BoolVal (b1 = b2)
  | ListVal l1, ListVal l2 -> k @@ BoolVal (l1 = l2)
  | v1, v2 ->
    failwith
    @@ "eq: compared expressions type mismatch: "
    ^ value_type v1
    ^ ", "
    ^ value_type v2

and match_eval e1 cases env k =
  eval e1 env @@ fun v1 ->
  let rec findMatch cases =
    match cases with
    | [] -> None
    | (e2, body) :: cases ->
      (match e2 with
      | Cons (Var h, Empty) ->
        (match v1 with
        | ListVal [ hv ] ->
          let env = ext env h hv in
          Some (body, env)
        | _ -> findMatch cases)
      | Cons (Var h, Var tl) ->
        (match v1 with
        | ListVal (hv :: tlv) ->
          let env = ext env h hv in
          let env = ext env tl (ListVal tlv) in
          Some (body, env)
        | _ -> findMatch cases)
      | Var x ->
        let env = ext env x v1 in
        Some (body, env)
      | _ ->
        let v2 = eval e2 env (fun x -> x) in
        if v1 = v2 then Some (body, env) else findMatch cases)
  in
  match findMatch cases with
  | None -> failwith "match: failed"
  | Some (body, env) -> eval body env k
;;

let eval exp env =
  let id x = x in
  eval exp env id
;;
