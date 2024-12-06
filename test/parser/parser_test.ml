let test_integer_literal () =
  let lexer = Lexer.new_lexer "5" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_integer_literal parser with
  | _, None -> Alcotest.fail "Failed to parse integer literal"
  | _, Some expr -> (
      match expr with
      | Ast.IntegerLiteral { value; token = Token.INT literal } ->
          Alcotest.(check int64) "integer value" 5L value;
          Alcotest.(check string) "token literal" "5" literal
      | _ -> Alcotest.fail "Expression is not an integer literal")

let test_identifier () =
  let lexer = Lexer.new_lexer "foobar" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_identifier parser with
  | _, None -> Alcotest.fail "Failed to parse identifier"
  | _, Some expr -> (
      match expr with
      | Ast.Identifier { value; token = Token.IDENT identifier } ->
          Alcotest.(check string) "identifier value" "foobar" value;
          Alcotest.(check string) "token literal" "foobar" identifier
      | _ -> Alcotest.fail "Expression is not an identifier")

let test_string_literal () =
  let lexer = Lexer.new_lexer "\"foobar\"" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_string_literal parser with
  | _, None -> Alcotest.fail "Failed to parse string literal"
  | _, Some expr -> (
      match expr with
      | Ast.StringLiteral { value; token = Token.STRING literal } ->
          Alcotest.(check string) "string value" "foobar" value;
          Alcotest.(check string) "token literal" "foobar" literal
      | _ -> Alcotest.fail "Expression is not a string literal")

let test_boolean_literal () =
  let lexer = Lexer.new_lexer "true" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_boolean_literal parser with
  | _, None -> Alcotest.fail "Failed to parse boolean literal"
  | _, Some expr -> (
      match expr with
      | Ast.BooleanLiteral { value; token = Token.TRUE } ->
          Alcotest.(check bool) "boolean value" true value
      | _ -> Alcotest.fail "Expression is not a boolean literal")

let test_let_statement () =
  let lexer = Lexer.new_lexer "let myVar = anotherVar;" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_let_statement parser with
  | _, None -> Alcotest.fail "Failed to parse let statement"
  | _, Some stmt -> (
      match stmt with
      | Ast.LetStatement { token = Token.LET; name; value } -> (
          match value with
          | Ast.Identifier { token = Token.IDENT id_token; value = id_value } ->
              Alcotest.(check string) "name value" "myVar" name.value;
              Alcotest.(check string) "identifier token" "anotherVar" id_token;
              Alcotest.(check string) "identifier value" "anotherVar" id_value
          | _ -> Alcotest.fail "Value is not an identifier")
      | _ -> Alcotest.fail "Statement is not a let statement")

let test_let_statement_with_integer_literal () =
  let lexer = Lexer.new_lexer "let myVar = 5;" in
  let parser = Parser.new_parser lexer in
  match Parser.parse_let_statement parser with
  | _, None -> Alcotest.fail "Failed to parse let statement"
  | _, Some stmt -> (
      match stmt with
      | Ast.LetStatement { token = Token.LET; name; value } -> (
          match value with
          | Ast.IntegerLiteral { token = Token.INT literal; value = int_value }
            ->
              Alcotest.(check string) "name value" "myVar" name.value;
              Alcotest.(check string) "integer token" "5" literal;
              Alcotest.(check int64) "integer value" 5L int_value
          | _ -> Alcotest.fail "Value is not an integer literal")
      | _ -> Alcotest.fail "Statement is not a let statement")

let () =
  Alcotest.run "Parser"
    [
      ( "parse literals",
        [
          Alcotest.test_case "integer literal" `Quick test_integer_literal;
          Alcotest.test_case "identifier" `Quick test_identifier;
          Alcotest.test_case "string literal" `Quick test_string_literal;
          Alcotest.test_case "boolean literal" `Quick test_boolean_literal;
        ] );
      ( "parse statements",
        [
          Alcotest.test_case "let statement" `Quick test_let_statement;
          Alcotest.test_case "let statement with integer literal" `Quick
            test_let_statement_with_integer_literal;
        ] );
    ]
