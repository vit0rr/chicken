let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let test_basic_program () =
  let input =
    {|
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {x + y;};
    let result = add(five, ten);

    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
    "foobar"
    "foo bar"
    [1, 2]
    {"foo": "bar"}
  |}
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
      (Token.BANG, "!");
      (Token.MINUS, "-");
      (Token.SLASH, "/");
      (Token.ASTERISK, "*");
      (Token.INT, "5");
      (Token.SEMICOLON, ";");
      (Token.INT, "5");
      (Token.LT, "<");
      (Token.INT, "10");
      (Token.GT, ">");
      (Token.INT, "5");
      (Token.SEMICOLON, ";");
      (Token.IF, "if");
      (Token.LPAREN, "(");
      (Token.INT, "5");
      (Token.LT, "<");
      (Token.INT, "10");
      (Token.RPAREN, ")");
      (Token.LBRACE, "{");
      (Token.RETURN, "return");
      (Token.TRUE, "true");
      (Token.SEMICOLON, ";");
      (Token.RBRACE, "}");
      (Token.ELSE, "else");
      (Token.LBRACE, "{");
      (Token.RETURN, "return");
      (Token.FALSE, "false");
      (Token.SEMICOLON, ";");
      (Token.RBRACE, "}");
      (Token.INT, "10");
      (Token.EQ, "==");
      (Token.INT, "10");
      (Token.SEMICOLON, ";");
      (Token.INT, "10");
      (Token.NOT_EQ, "!=");
      (Token.INT, "9");
      (Token.SEMICOLON, ";");
      (Token.STRING, "foobar");
      (Token.STRING, "foo bar");
      (Token.LBRACKET, "[");
      (Token.INT, "1");
      (Token.COMMA, ",");
      (Token.INT, "2");
      (Token.RBRACKET, "]");
      (Token.LBRACE, "{");
      (Token.STRING, "foo");
      (Token.COLON, ":");
      (Token.STRING, "bar");
      (Token.RBRACE, "}");
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
