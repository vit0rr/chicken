type token =
  | ILLEGAL
  | EOF
  (* Identifiers and literals *)
  | IDENT of string
  | INT of string
  | STRING of string
  (* Operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | OR
  (* Equality operators *)
  | EQ
  | NOT_EQ
  (* Delimiters *)
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  (* Keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN

let token_to_string = function
  | ILLEGAL -> "ILLEGAL"
  | EOF -> "EOF"
  | IDENT a -> "IDENT " ^ a
  | INT a -> "INT " ^ a
  | STRING a -> "STRING " ^ a
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | EQ -> "EQ"
  | NOT_EQ -> "NOT_EQ"
  | OR -> "OR"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"

let keywords =
  [
    ("fn", FUNCTION);
    ("let", LET);
    ("if", IF);
    ("else", ELSE);
    ("true", TRUE);
    ("false", FALSE);
    ("return", RETURN);
  ]

let tokens_eq tok_a tok_b =
  match (tok_a, tok_b) with
  | IDENT a, IDENT b -> a = b
  | tok_a, tok_b -> tok_a = tok_b

let pretty_print ppf tok = Fmt.pf ppf "Token %s" (token_to_string tok)
let lookup_ident str = try List.assoc str keywords with Not_found -> IDENT str
