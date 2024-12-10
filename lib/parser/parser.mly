%{
    open Ast
%}

%token <string> IDENT
%token <string> INT
%token LET ASSIGN SEMICOLON EOF

%start <Ast.program> prog

%%

prog:
    | statements EOF { { statements = $1 } }
;

statements:
    | /* empty */     { [] }
    | s = statement l = statements { s :: l }
;

statement:
    | let_statement { $1 }
;

let_statement:
    | LET name = IDENT ASSIGN value = expression SEMICOLON
        { LetStatement {
            token = Token.new_token Token.LET "let";
            name = { 
                token = Token.new_token Token.IDENT name;
                value = name 
            };
            value = value
        } }
;

expression:
    | i = INT { IntegerLiteral {
        token = Token.new_token Token.INT i;
        value = Int64.of_string i
    } }
;
