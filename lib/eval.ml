open Syntax

let rec eval e =
  let ename = exp_name e in
  match e with
  | IntLit n -> IntVal n
  | BoolLit b -> BoolVal b
  | Plus (e1, e2) ->
    (match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
    | _ -> failwith "integer values expected")
  | Minus (e1, e2) ->
    (match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> IntVal (n1 - n2)
    | _ -> failwith "integer values expected")
  | Times (e1, e2) ->
    (match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> IntVal (n1 * n2)
    | _ -> failwith "integer values expected")
  | Div (e1, e2) ->
    (match eval e1, eval e2 with
    | IntVal n1, IntVal n2 -> IntVal (n1 / n2)
    | _ -> failwith "integer values expected")
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
  | _ -> failwith @@ String.concat "" [ "not implemented: "; ename ]
;;
