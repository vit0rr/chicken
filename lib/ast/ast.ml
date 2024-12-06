type node = { token_literal : unit -> string }
type identifier = { token : Token.token; value : string }
type statement = { node : node }
type block_statement = { token : Token.token; statements : statement list }

type expressions =
  | Identifier of identifier
  | IntegerLiteral of { token : Token.token; value : int64 }
  | StringLiteral of { token : Token.token; value : string }
  | BooleanLiteral of { token : Token.token; value : bool }
  | PrefixExpression of {
      token : Token.token;
      operator : string;
      right : expressions;
    }
  | InfixExpression of {
      token : Token.token;
      left : expressions;
      operator : string;
      right : expressions;
    }
  | IfExpression of {
      token : Token.token;
      condition : expressions;
      consequence : block_statement;
      alternative : block_statement;
    }
  | FunctionLiteral of {
      token : Token.token;
      parameters : identifier list;
      body : block_statement;
    }
  | CallExpression of {
      token : Token.token;
      func : expressions;
      arguments : expressions list;
    }
  | ArrayLiteral of { token : Token.token; elements : expressions list }
  | IndexExpression of {
      token : Token.token;
      left : expressions;
      index : expressions;
    }
  | HashLiteral of {
      token : Token.token;
      pairs : (expressions * expressions) list;
    }

and statements =
  | LetStatement of {
      token : Token.token;
      name : identifier;
      value : expressions;
    }
  | ReturnStatement of {
      token : Token.token;
      return_value : expressions option;
    }
  | ExpressionStatement of {
      token : Token.token;
      expression : expressions option;
    }
  | BlockStatement of { token : Token.token; statements : statements list }

type program = { statements : statements list }

let rec expression_to_string (expr : expressions) : string =
  match expr with
  | Identifier id -> id.value
  | IntegerLiteral { value; _ } -> Int64.to_string value
  | StringLiteral { value; _ } -> Printf.sprintf "\"%s\"" value
  | BooleanLiteral { value; _ } -> string_of_bool value
  | PrefixExpression { operator; right; _ } ->
      Printf.sprintf "(%s%s)" operator (expression_to_string right)
  | InfixExpression { left; operator; right; _ } ->
      Printf.sprintf "(%s %s %s)"
        (expression_to_string left)
        operator
        (expression_to_string right)
  | IfExpression { condition; consequence; alternative; _ } ->
      Printf.sprintf "if %s %s else %s"
        (expression_to_string condition)
        (block_statement_to_string consequence)
        (block_statement_to_string alternative)
  | FunctionLiteral { parameters; body; _ } ->
      let params =
        parameters |> List.map (fun p -> p.value) |> String.concat ", "
      in
      Printf.sprintf "fn(%s) %s" params (block_statement_to_string body)
  | CallExpression { func; arguments; _ } ->
      let args =
        arguments |> List.map expression_to_string |> String.concat ", "
      in
      Printf.sprintf "%s(%s)" (expression_to_string func) args
  | ArrayLiteral { elements; _ } ->
      let elems =
        elements |> List.map expression_to_string |> String.concat ", "
      in
      Printf.sprintf "[%s]" elems
  | IndexExpression { left; index; _ } ->
      Printf.sprintf "(%s[%s])"
        (expression_to_string left)
        (expression_to_string index)
  | HashLiteral { pairs; _ } ->
      let pair_strings =
        pairs
        |> List.map (fun (k, v) ->
               Printf.sprintf "%s: %s" (expression_to_string k)
                 (expression_to_string v))
        |> String.concat ", "
      in
      Printf.sprintf "{%s}" pair_strings

and statement_to_string (stmt : statements) : string =
  match stmt with
  | LetStatement { name; value; _ } ->
      Printf.sprintf "let %s = %s;" name.value (expression_to_string value)
  | ReturnStatement { return_value; _ } -> (
      match return_value with
      | Some expr -> Printf.sprintf "return %s;" (expression_to_string expr)
      | None -> "return;")
  | ExpressionStatement { expression; _ } -> (
      match expression with
      | Some expr -> expression_to_string expr
      | None -> "")
  | BlockStatement { statements; _ } ->
      statements |> List.map statement_to_string |> String.concat "\n"

and block_statement_to_string (block : block_statement) : string =
  Printf.sprintf "{ %s }"
    (block.statements
    |> List.map (fun s -> s.node.token_literal ())
    |> String.concat " ")

let program_to_string (program : program) : string =
  program.statements |> List.map statement_to_string |> String.concat "\n"
