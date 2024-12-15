open Chicken__Parser

exception Invalid_token of string

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t' | '\r')]
let digit = [%sedlex.regexp? '0' .. '9']
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? lower | upper]
let identifier = [%sedlex.regexp? (lower | '_'), Star (alpha | digit | '_')]
let integer = [%sedlex.regexp? Plus digit]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | "let" -> LET
  | "=" -> ASSIGN
  | "+" -> PLUS
  | "-" -> MINUS
  | integer -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | identifier -> IDENT (Sedlexing.Utf8.lexeme buf)
  | eof -> EOF
  | _ -> raise (Invalid_token (Sedlexing.Utf8.lexeme buf))

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string f string =
  provider (Sedlexing.Utf8.from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f
