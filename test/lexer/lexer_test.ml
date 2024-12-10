let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let test_basic_program () =
  let input =
    "\n\
    \  let five = 5;\n\
    \  let ten = 10;\n\
     let add = fn(x, y) {x + y;};\n\
     let result = add(five, ten);\n\
    \  "
  in
  let expected =
    [
      (Token.LET, "let");
      (Token.IDENT, "five");
      (Token.ASSIGN, "=");
      (Token.INT, "5");
      (Token.SEMICOLON, ";");
      (Token.LET, "let");
      (Token.IDENT, "ten");
      (Token.ASSIGN, "=");
      (Token.INT, "10");
      (Token.SEMICOLON, ";");
      (Token.LET, "let");
      (Token.IDENT, "add");
      (Token.ASSIGN, "=");
      (Token.FUNCTION, "fn");
      (Token.LPAREN, "(");
      (Token.IDENT, "x");
      (Token.COMMA, ",");
      (Token.IDENT, "y");
      (Token.RPAREN, ")");
      (Token.LBRACE, "{");
      (Token.IDENT, "x");
      (Token.PLUS, "+");
      (Token.IDENT, "y");
      (Token.SEMICOLON, ";");
      (Token.RBRACE, "}");
      (Token.SEMICOLON, ";");
      (Token.LET, "let");
      (Token.IDENT, "result");
      (Token.ASSIGN, "=");
      (Token.IDENT, "add");
      (Token.LPAREN, "(");
      (Token.IDENT, "five");
      (Token.COMMA, ",");
      (Token.IDENT, "ten");
      (Token.RPAREN, ")");
      (Token.SEMICOLON, ";");
      (Token.EOF, "");
    ]
  in
  let lexer = Lexer.new_lexer input in
  List.iter
    (fun (expected_type, expected_literal) ->
      match Lexer.next_token lexer with
      | Some token ->
          Alcotest.(check token_testable)
            (Printf.sprintf "token type for '%s'" expected_literal)
            { Token.token_type = expected_type; literal = expected_literal }
            token
      | None -> Alcotest.fail "Unexpected end of input")
    expected

let test_string_literals () =
  let input = "\"hello world\"" in
  let expected = [ (Token.STRING, "hello world"); (Token.EOF, "") ] in
  let lexer = Lexer.new_lexer input in
  List.iter
    (fun (expected_type, expected_literal) ->
      match Lexer.next_token lexer with
      | Some token ->
          Alcotest.(check token_testable)
            (Printf.sprintf "token type for '%s'" expected_literal)
            { Token.token_type = expected_type; literal = expected_literal }
            token
      | None -> Alcotest.fail "Unexpected end of input")
    expected

let () =
  Alcotest.run "Lexer"
    [
      ( "tokens",
        [
          Alcotest.test_case "basic program" `Quick test_basic_program;
          Alcotest.test_case "string literals" `Quick test_string_literals;
        ] );
    ]
