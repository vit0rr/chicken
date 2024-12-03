type token =
  | ILLEGAL
  | EOF
  | IDENT of string
  | INT of string
  | STRING of string
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
  | EQ
  | NOT_EQ

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

let lookup_ident str = try List.assoc str keywords with Not_found -> IDENT str
