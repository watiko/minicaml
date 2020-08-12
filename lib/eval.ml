open Syntax

type env = Syntax.env

let unsafeParse s =
  let result = Parser.(parse main (explode s)) in
  match result with
  | Some exp -> exp
  | None -> failwith @@ "parse error: " ^ s
;;

let emptyenv () : env = []
let ext (env : env) x v = (x, v) :: env

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

let rec lookup x env =
  match env with
  | [] -> None
  | (y, v) :: rest -> if x = y then Some v else lookup x rest
;;

let rec eval e env =
  let binop f e1 e2 env =
    match eval e1 env, eval e2 env with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  let condop f e1 e2 env =
    match eval e1 env, eval e2 env with
    | IntVal n1, IntVal n2 -> BoolVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | Var x ->
    (match lookup x env with
    | Some v -> v
    | None -> failwith @@ "lookup failed with key: " ^ x)
  | IntLit n -> IntVal n
  | BoolLit b -> BoolVal b
  | StrLit s -> StrVal s
  | Fun (x, e1) -> FunVal (x, e1, env)
  | App (e1, e2) ->
    let fn = eval e1 env in
    let arg = eval e2 env in
    (match fn with
    | FunVal (x, body, fenv) ->
      let fenv = ext fenv x arg in
      eval body fenv
    | RecFunVal (f, x, body, fenv) ->
      let fenv = ext fenv x arg in
      let fenv = ext fenv f fn in
      eval body fenv
    | _ -> failwith "app: function value required")
  | Let (x, e1, e2) ->
    let env = ext env x (eval e1 env) in
    eval e2 env
  | LetRec (f, x, e1, e2) ->
    let env = ext env f (RecFunVal (f, x, e1, env)) in
    eval e2 env
  | Plus (e1, e2) -> binop ( + ) e1 e2 env
  | Minus (e1, e2) -> binop ( - ) e1 e2 env
  | Times (e1, e2) -> binop ( * ) e1 e2 env
  | Div (e1, e2) -> binop ( / ) e1 e2 env
  | Unit -> UnitVal
  | Greater (e1, e2) -> condop ( > ) e1 e2 env
  | Less (e1, e2) -> condop ( < ) e1 e2 env
  | Eq (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
    | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
    | ListVal l1, ListVal l2 -> BoolVal (l1 = l2)
    | v1, v2 ->
      failwith
      @@ "eq: compared expressions type mismatch: "
      ^ value_type v1
      ^ ", "
      ^ value_type v2)
  | If (c, e1, e2) ->
    (match eval c env with
    | BoolVal true -> eval e1 env
    | BoolVal false -> eval e2 env
    | v -> failwith @@ "if: cond type is not bool but got: " ^ value_type v)
  | Empty -> ListVal []
  | Cons (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | v1, ListVal v2 -> ListVal (v1 :: v2)
    | v1, v2 ->
      failwith
      @@ "cons: required list type but got: "
      ^ value_type v1
      ^ ", "
      ^ value_type v2)
  | FailWith e ->
    (match eval e env with
    | StrVal s -> failwith s
    | v -> failwith @@ "failwith: required str type but got: " ^ value_type v)
  | Match (e1, cases) ->
    let v1 = eval e1 env in
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
          let v2 = eval e2 env in
          if v1 = v2 then Some (body, env) else findMatch cases)
    in
    (match findMatch cases with
    | None -> failwith "match: failed"
    | Some (body, env) -> eval body env)
;;
