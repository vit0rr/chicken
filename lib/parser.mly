(* parser.mly *)
%{
    open Ast
%}

%token EOF
%token <string> IDENT
%token <int> INT
%token ASSIGN PLUS MINUS
%token LET

%start <Ast.program> program

%%

program:
| stmts = list(stmt) EOF 
    { {statements = stmts} }
stmt:
| LET name = IDENT ASSIGN e = expr 
    { Let { 
        name = name;
        value = e 
    } }

expr:
| i = INT 
    { Literal (LitInt (Int64.of_int i)) }
| id = IDENT 
    { Identifier id }
| e1 = expr PLUS e2 = expr 
    { Infix { 
        left = e1;
        operator = "+";
        right = e2 
    } }
| e1 = expr MINUS e2 = expr 
    { Infix { 
        left = e1;
        operator = "-";
        right = e2 
    } }