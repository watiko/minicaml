type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp
  | Times of exp * exp
  | If of exp * exp * exp
  | Eq of exp * exp

type value =
  | IntVal of int
  | BoolVal of bool
