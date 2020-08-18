module Syntax = Minicaml.Syntax
module Parser = Minicaml.Parser
module Type = Minicaml.Type
module Eval = Minicaml.Eval

module ResultSyntax = struct
  let bind r f =
    match r with
    | Ok x -> f x
    | Error _ as v -> v
  ;;

  let ( let* ) r f = bind r f
  let pure x = Ok x
end

let parseResult s =
  match Parser.(parse main (explode s)) with
  | None -> Error "parse failed"
  | Some e -> Ok e
;;

let inferResult e =
  try Ok Type.(infer (defaultenv ()) e) with
  | e -> Error (Printexc.to_string e)
;;

let evalResult e =
  try Ok Eval.(eval e (defaultenv ())) with
  | e -> Error (Printexc.to_string e)
;;

let run s =
  let open ResultSyntax in
  let* e = parseResult s in
  let* _, t = inferResult e in
  let* v = evalResult e in
  pure (v, t)
;;

let () =
  let () = Fmt.pr "starting REPL...\n" in
  try
    while true do
      let () = Fmt.pr "> %a" Fmt.flush () in
      let line = input_line stdin in
      match run line with
      | Error message -> Fmt.epr "error: %s\n%a" message Fmt.flush ()
      | Ok (v, t) ->
        Fmt.pr "type:  %a\n%a" Type.pprint_type t Fmt.flush ();
        Fmt.pr "value: %a\n%a" Syntax.pprint_value_simplified v Fmt.flush ()
    done
  with
  | End_of_file -> ()
;;
