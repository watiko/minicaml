open Syntax

type env = Syntax.env

let emptyenv () = ([] : env)
let ext (env : env) x v = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> None
  | (y, v) :: rest -> if x = y then Some v else lookup x rest
;;

let rec eval e =
  let ename = exp_name e in
  let binop f e1 e2 =
    match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit n -> IntVal n
  | BoolLit b -> BoolVal b
  | Plus (e1, e2) -> binop ( + ) e1 e2
  | Minus (e1, e2) -> binop ( - ) e1 e2
  | Times (e1, e2) -> binop ( * ) e1 e2
  | Div (e1, e2) -> binop ( / ) e1 e2
  | Eq (e1, e2) ->
    (match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
    | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
    | _ -> failwith "eq: compared expressions type mismatch")
  | If (c, e1, e2) ->
    (match eval c with
    | BoolVal true -> eval e1
    | BoolVal false -> eval e2
    | _ -> failwith "if: cond type is not bool")
  | _ -> failwith @@ "not implemented: " ^ ename
;;
