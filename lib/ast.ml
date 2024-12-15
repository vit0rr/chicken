type literal =
  | LitInt of int64
  | LitString of string
  | LitBool of bool
  | LitArray of expr list
  | LitHash of (expr * expr) list
[@@deriving show]

and expr =
  | Identifier of string
  | Literal of literal
  | Prefix of { operator : string; right : expr }
  | Infix of { left : expr; operator : string; right : expr }
  | If of { condition : expr; consequence : block; alternative : block }
  | Function of { parameters : string list; body : block }
  | Call of { func : expr; arguments : expr list }
  | Index of { left : expr; index : expr }
[@@deriving show]

and stmt =
  | Let of { name : string; value : expr }
  | Return of expr option
  | Expression of expr option
  | Block of block
[@@deriving show]

and block = stmt list [@@deriving show]

type program = { statements : stmt list } [@@deriving show]
