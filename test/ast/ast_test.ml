(* let test_let_statement () =
  let program =
    {
      Ast.statements =
        [
          LetStatement
            {
              token = Token.LET;
              name = { token = Token.IDENT "myVar"; value = "myVar" };
              value =
                Identifier
                  { token = Token.IDENT "anotherVar"; value = "anotherVar" };
            };
        ];
    }
  in
  Alcotest.(check string)
    "let statement" "let myVar = anotherVar;"
    (Ast.program_to_string program)

let test_return_statement () =
  let program =
    {
      Ast.statements =
        [
          ReturnStatement
            {
              token = Token.RETURN;
              return_value =
                Some (IntegerLiteral { token = Token.INT "5"; value = 5L });
            };
        ];
    }
  in
  Alcotest.(check string)
    "return statement" "return 5;"
    (Ast.program_to_string program)

let test_expression_statement () =
  let program =
    {
      Ast.statements =
        [
          ExpressionStatement
            {
              token = Token.IDENT "x";
              expression =
                Some
                  (InfixExpression
                     {
                       token = Token.PLUS;
                       left =
                         Identifier { token = Token.IDENT "x"; value = "x" };
                       operator = "+";
                       right =
                         Identifier { token = Token.IDENT "y"; value = "y" };
                     });
            };
        ];
    }
  in
  Alcotest.(check string)
    "expression statement" "(x + y)"
    (Ast.program_to_string program)

let () =
  Alcotest.run "AST"
    [
      ( "string representation",
        [
          Alcotest.test_case "let statement" `Quick test_let_statement;
          Alcotest.test_case "return statement" `Quick test_return_statement;
          Alcotest.test_case "expression statement" `Quick
            test_expression_statement;
        ] );
    ] *)
