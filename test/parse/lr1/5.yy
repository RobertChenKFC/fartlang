%token L_BRACK R_BRACK L_BRACE R_BRACE L_PAREN R_PAREN EQ ADD SUB COMMA SEMICOL
%token CONSTANT INT FLOAT IF ELSE WHILE IDENTIFIER

%define lr.type canonical-lr

%start function

%%

function
  : type IDENTIFIER L_PAREN param_list R_PAREN L_BRACE block R_BRACE
  ;

param_list
  : param_list COMMA param
  | param
  ;

param
  : type IDENTIFIER
  ;

type
  : INT
  | FLOAT
  ;

block
  : stmt_list
  ;

stmt_list
  : stmt SEMICOL stmt_list
  | stmt
  ;

stmt
  : L_BRACE block R_BRACE
  | var EQ expr SEMICOL
  | WHILE L_PAREN test R_PAREN stmt
  | IF L_PAREN test R_PAREN stmt
  | IF L_PAREN test R_PAREN stmt ELSE stmt
  ;

test
  : expr
  | var EQ expr
  ;

var
  : IDENTIFIER
  | IDENTIFIER L_BRACK expr R_BRACK
  ;

expr
  : expr ADD term
  | expr SUB term
  | term
  ;

term
  : var
  | CONSTANT
  | L_PAREN expr R_PAREN
  ;

%%
