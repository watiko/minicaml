open Syntax

type env = Syntax.env

let emptyenv () = ([] : env)
let ext (env : env) x v = (x, v) :: env
let defaultenv = emptyenv

let rec lookup x env =
  match env with
  | [] -> None
  | (y, v) :: rest -> if x = y then Some v else lookup x rest
;;

let rec eval e env =
  let ename = exp_name e in
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
    | _ -> failwith "eq: compared expressions type mismatch")
  | If (c, e1, e2) ->
    (match eval c env with
    | BoolVal true -> eval e1 env
    | BoolVal false -> eval e2 env
    | _ -> failwith "if: cond type is not bool")
  | _ -> failwith @@ "not implemented: " ^ ename
;;
