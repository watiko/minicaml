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

let eat_script () =
  let buffer = ref "" in
  let next = ref true in
  let terminate = Str.regexp_string ";;" in
  let () =
    while !next do
      let line = input_line stdin in
      let pos =
        try
          let pos = Str.search_forward terminate line 0 in
          Some pos
        with
        | Not_found -> None
      in
      let content =
        match pos with
        | None -> line
        | Some pos -> String.sub line 0 pos
      in
      let () = buffer := !buffer ^ "\n" ^ content in
      let () = if Option.is_some pos then next := false else () in
      ()
    done
  in
  !buffer
;;

let () =
  let () = Fmt.pr "starting REPL...\n" in
  try
    while true do
      let () = Fmt.pr "> %a" Fmt.flush () in
      let buffer = eat_script () in
      match run buffer with
      | Error message -> Fmt.epr "error: %s\n%a" message Fmt.flush ()
      | Ok (v, t) ->
        Fmt.pr "type:  %a\n%a" Type.pprint_type t Fmt.flush ();
        Fmt.pr "value: %a\n%a" Syntax.pprint_value_simplified v Fmt.flush ()
    done
  with
  | End_of_file -> ()
;;
