let explode s = List.init (String.length s) (String.get s)
let implode cs = String.init (List.length cs) (List.nth cs)

let%test _ = [ 'a'; 'b'; 'c' ] = explode "abc"
let%test _ = "abc" = implode [ 'a'; 'b'; 'c' ]
