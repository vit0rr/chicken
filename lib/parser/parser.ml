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
  peek_token : Token.token;
}

let new_parser (lexer : Lexer.lexer) : parser =
  let le, tok = Lexer.next_char lexer in
  let _, tk = Lexer.next_char le in
  { lexer = le; errors = []; current_token = tok; peek_token = tk }

let rec errors_to_string errors =
  match errors with
  | [] -> ""
  | h :: [] -> h
  | h :: t -> h ^ ", " ^ errors_to_string t

let error_parse parser message =
  {
    lexer = parser.lexer;
    errors =
      parser.errors
      @ [ " " ^ message ^ "@" ^ Token.token_to_string parser.current_token ];
    current_token = parser.current_token;
    peek_token = parser.peek_token;
  }

let next_token parser =
  let lexer, next_token = Lexer.next_char parser.lexer in
  {
    lexer;
    errors = parser.errors;
    current_token = parser.peek_token;
    peek_token = next_token;
  }

let peek_precedence parser =
  precedence_to_int (precedence_of_token parser.peek_token)

let parse_integer_literal parser =
  match parser.current_token with
  | Token.INT literal ->
      ( parser,
        Some
          (Ast.IntegerLiteral
             { value = Int64.of_string literal; token = parser.current_token })
      )
  | _ -> (parser, None)

let parse_identifier parser =
  match parser.current_token with
  | Token.IDENT identifier ->
      ( parser,
        Some
          (Ast.Identifier { value = identifier; token = parser.current_token })
      )
  | _ -> (parser, None)

let parse_string_literal parser =
  match parser.current_token with
  | Token.STRING literal ->
      ( parser,
        Some
          (Ast.StringLiteral { value = literal; token = parser.current_token })
      )
  | _ -> (parser, None)

let parse_boolean_literal parser =
  match parser.current_token with
  | Token.TRUE ->
      ( parser,
        Some (Ast.BooleanLiteral { value = true; token = parser.current_token })
      )
  | Token.FALSE ->
      ( parser,
        Some
          (Ast.BooleanLiteral { value = false; token = parser.current_token })
      )
  | _ -> (parser, None)

let parse_expression parser =
  let parser, expr = parse_integer_literal parser in
  match expr with
  | Some expr -> (parser, Some expr)
  | None -> parse_identifier parser

let parse_let_statement parser =
  let parser, name_opt = parse_identifier parser in
  let parser, value_opt = parse_expression parser in
  match (name_opt, value_opt) with
  | Some (Identifier name), Some value ->
      ( parser,
        Some (Ast.LetStatement { token = parser.current_token; name; value }) )
  | _ -> (parser, None)

let parse_return_statement parser =
  let parser, value_opt = parse_expression parser in
  ( parser,
    Some
      (Ast.ReturnStatement
         { token = parser.current_token; return_value = value_opt }) )

let parse_expression_statement parser =
  let parser, expr = parse_expression parser in
  ( parser,
    Some
      (Ast.ExpressionStatement
         { token = parser.current_token; expression = expr }) )

let parse_statement parser =
  match parser.current_token with
  | Token.LET -> parse_let_statement parser
  | Token.RETURN -> parse_return_statement parser
  | _ -> parse_expression_statement parser

let parse_block_statement parser =
  let token = parser.current_token in
  let parser = next_token parser in
  let rec parse_statements parser statements =
    match parser.current_token with
    | Token.RBRACE ->
        let parser = next_token parser in
        (parser, Some (Ast.BlockStatement { token; statements }))
    | Token.EOF -> (parser, None)
    | Token.SEMICOLON -> parse_statements (next_token parser) statements
    | _ -> (
        let parser, stmt_opt = parse_statement parser in
        match stmt_opt with
        | Some stmt ->
            let parser = next_token parser in
            parse_statements parser (statements @ [ stmt ])
        | None ->
            let parser = next_token parser in
            parse_statements parser statements)
  in
  parse_statements parser []
