type node = { token_literal : unit -> string; string_ : unit -> string }
type statement = { node : node }
type program = { statements : statement list }
type expression = { node : node }
type identifier = { token : Token.token; value : string }

type let_statement = {
  token : Token.token;
  name : identifier;
  value : expression;
}

let statement_to_string (stmt : statement) : string = stmt.node.string_ ()

let program_to_string (program : program) : string =
  program.statements |> List.map statement_to_string |> String.concat "\n"
