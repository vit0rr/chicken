(* let token_testable = Alcotest.testable Token.pretty_print Token.tokens_eq

let test_parse_let_statement () =
  let input = "let five = 5" in
  let lexbuf = Lexing.from_string input in
  let parse = Parser.prog Lexer.token lexbuf

(* let () =
  Alcotest.run "Lexer"
    [
      ( "tokens",
        [ Alcotest.test_case "basic program" `Quick test_parse_let_statement ] );
    ] *) *)
