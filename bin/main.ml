let read_until_eof () =
  let buffer = Buffer.create 2048 in
  let rec read_loop () =
    let line = try Some (read_line ()) with End_of_file -> None in
    match line with
    | None -> Buffer.contents buffer
    | Some line ->
        Buffer.add_string buffer (line ^ "\n");
        read_loop ()
  in
  read_loop ()

let show_value = function
  | Chicken.Evaluator.VInt n -> Int64.to_string n
  | Chicken.Evaluator.VString s -> s
  | Chicken.Evaluator.VBool b -> string_of_bool b
  | _ -> "null"

let () =
  read_until_eof ()
  |> Chicken.Lexer.from_string Chicken.Parser.program
  |> Chicken.Evaluator.eval_program |> show_value |> print_endline
