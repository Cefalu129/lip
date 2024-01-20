%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left MUL DIV

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | MINUS; e = expr { Sub(Const 0,e) }
  | LPAREN; e=expr; RPAREN {e}
;
