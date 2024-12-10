type token_type =
  | ILLEGAL
  | EOF
  (* identifers and literals *)
  | IDENT
  | INT
  | STRING
  (* operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | EQ
  | NOT_EQ
  (* delimeters *)
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COLON
  (* keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN

type token = { token_type : token_type; literal : string }

let token_to_string token =
  match token.token_type with
  | ILLEGAL -> "ILLEGAL:" ^ token.literal
  | EOF -> "EOF"
  (* identifiers and literals *)
  | IDENT -> "IDENT:" ^ token.literal
  | INT -> "INT:" ^ token.literal
  | STRING -> "STRING:" ^ token.literal
  (* operators *)
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NOT_EQ -> "NOT_EQ"
  (* delimeters *)
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | COLON -> "COLON"
  (* keywords *)
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"

let keywords = function
  | "fn" -> FUNCTION
  | "let" -> LET
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "return" -> RETURN
  | _ -> ILLEGAL

let is_semicolon token = token.token_type = SEMICOLON
let new_token token_type literal = { token_type; literal }

let tokens_eq token_a token_b =
  token_a.literal = token_b.literal && token_a.token_type = token_b.token_type

let pretty_print ppf tok = Fmt.pf ppf "Token %s" (token_to_string tok)
