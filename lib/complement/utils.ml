let rec fold_right1 f = function
  | [] -> invalid_arg "fold_right1"
  | [ x ] -> x
  | x :: xs -> f x (fold_right1 f xs)
;;

let%test "x" = 15 = fold_right1 ( + ) [ 1; 2; 3; 4; 5 ]
