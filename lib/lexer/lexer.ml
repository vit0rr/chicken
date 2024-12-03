type lexer = { input : string; position : int; read_position : int; ch : char }

let null_char = '\x00'

let read_char lexer =
  let end_input = lexer.read_position >= String.length lexer.input in
  let new_ch lexer =
    match end_input with
    | true -> null_char
    | false -> String.get lexer.input lexer.read_position
  in
  {
    lexer with
    position = lexer.read_position;
    read_position = lexer.read_position + 1;
    ch = new_ch lexer;
  }

let new_lexer input =
  let lexer = { input; position = 0; read_position = 0; ch = null_char } in
  read_char lexer
