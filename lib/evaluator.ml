open Ast

type value =
  | VInt of int64
  | VString of string
  | VBool of bool
  | VArray of value list
  | VHash of (value * value) list
  | VFunction of {
      parameters : string list;
      body : Ast.block;
      env : environment;
    }
  | VNull

and environment = {
  store : (string, value) Hashtbl.t;
  outer : environment option;
}

let create_environment ?outer () = { store = Hashtbl.create 8; outer }

let rec lookup_var env name =
  match Hashtbl.find_opt env.store name with
  | Some value -> value
  | None -> (
      match env.outer with Some outer -> lookup_var outer name | None -> VNull)

let set_var env name value =
  Hashtbl.replace env.store name value;
  value

let rec eval_expr env = function
  | Ast.Literal (LitInt n) -> VInt n
  | Ast.Literal (LitString s) -> VString s
  | Ast.Literal (LitBool b) -> VBool b
  | Ast.Identifier name -> lookup_var env name
  | Ast.Infix { left; operator; right } -> (
      let left_val = eval_expr env left in
      let right_val = eval_expr env right in
      match (left_val, operator, right_val) with
      | VInt l, "+", VInt r -> VInt (Int64.add l r)
      | VInt l, "-", VInt r -> VInt (Int64.sub l r)
      | _ -> VNull)
  | Ast.Literal (LitArray arr) -> VArray (List.map (eval_expr env) arr)
  | Ast.Literal (LitHash pairs) ->
      VHash (List.map (fun (k, v) -> (eval_expr env k, eval_expr env v)) pairs)
  | Ast.Prefix _ -> VNull (* Placeholder *)
  | Ast.If _ -> VNull (* Placeholder *)
  | Ast.Function _ -> VNull (* Placeholder *)
  | Ast.Call _ -> VNull (* Placeholder *)
  | Ast.Index _ -> VNull (* Placeholder *)

let eval_stmt env = function
  | Ast.Let { name; value } ->
      let evaluated = eval_expr env value in
      set_var env name evaluated
  | Ast.Expression (Some expr) -> eval_expr env expr
  | Ast.Expression None -> VNull
  | Ast.Return _ -> VNull (* I'll implement this later *)
  | Ast.Block _ -> VNull (* I'll implement this later *)

let eval_program program =
  let env = create_environment () in
  let rec eval_statements = function
    | [] -> VNull
    | [ stmt ] -> eval_stmt env stmt
    | stmt :: rest ->
        let _ = eval_stmt env stmt in
        eval_statements rest
  in
  eval_statements program.Ast.statements
