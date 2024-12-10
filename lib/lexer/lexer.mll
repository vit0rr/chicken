{
open Token

exception LexingError of string
}

let whitespace = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

rule token = parse
  | whitespace+    { token lexbuf }  (* Skip whitespace *)
  | digit+ as num  { new_token INT num }
  | '"' ([^ '"']* as str) '"' { new_token STRING str }
  | "="           { new_token ASSIGN "=" }
  | "+"           { new_token PLUS "+" }
  | "-"           { new_token MINUS "-" }
  | "!"           { new_token BANG "!" }
  | "*"           { new_token ASTERISK "*" }
  | "/"           { new_token SLASH "/" }
  | "<"           { new_token LT "<" }
  | ">"           { new_token GT ">" }
  | "=="          { new_token EQ "==" }
  | "!="          { new_token NOT_EQ "!=" }
  | ","           { new_token COMMA "," }
  | ":"           { new_token COLON ":" }
  | ";"           { new_token SEMICOLON ";" }
  | "("           { new_token LPAREN "(" }
  | ")"           { new_token RPAREN ")" }
  | "{"           { new_token LBRACE "{" }
  | "}"           { new_token RBRACE "}" }
  | "["           { new_token LBRACKET "[" }
  | "]"           { new_token RBRACKET "]" }
  | "fn"          { new_token FUNCTION "fn" }
  | "let"         { new_token LET "let" }
  | "true"        { new_token TRUE "true" }
  | "false"       { new_token FALSE "false" }
  | "if"          { new_token IF "if" }
  | "else"        { new_token ELSE "else" }
  | "return"      { new_token RETURN "return" }
  | identifier as id { new_token IDENT id }
  | eof           { new_token EOF "" }
  | _ as c        { raise (LexingError (String.make 1 c)) }

{
let next_token lexbuf =
  try
    Some (token lexbuf)
  with
  | LexingError msg -> Some (new_token ILLEGAL msg)
  | _ -> None

let new_lexer input =
  Lexing.from_string input
}
