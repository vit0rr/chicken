type precedence =
  | Lowest
  | Equals
  | Or
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
  | Index

let precedence_to_int = function
  | Lowest -> 1
  | Equals -> 2
  | Or -> 3
  | LessGreater -> 4
  | Sum -> 5
  | Product -> 6
  | Prefix -> 7
  | Call -> 8
  | Index -> 9

let precedence_of_token = function
  | Token.EQ | Token.NOT_EQ -> Equals
  | Token.LT | Token.GT -> LessGreater
  | Token.OR -> Or
  | Token.PLUS | Token.MINUS -> Sum
  | Token.SLASH | Token.ASTERISK -> Product
  | Token.LPAREN -> Call
  | Token.LBRACE -> Index
  | Token.LBRACKET -> Index
  | _ -> Lowest

type parser = {
  lexer : Lexer.lexer;
  errors : string list;
  current_token : Token.token;
  peek_token : Token.token; (* TODO: prefix_parse_fns infix_parse_fns*)
}
