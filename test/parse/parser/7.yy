%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token LBRACE "{"
%token RBRACE "}"
%token LE "<="
%token LT "<"
%token GE ">="
%token GT ">"
%token EQ "=="
%token NE "!="
%token AND "&&"
%token OR "||"
%token NOT "!"
%token ASSIGN "="
%token COMMA ","
%token SEMICOL ";"
%token NUM FLOAT_NUM INT FLOAT IF ELSE WHILE RETURN IDENTIFIER

%start functionList

%%

functionList
    : functionList function
    | 
    ;

function
    : type IDENTIFIER "(" paramList ")" block
    ;

block
    : "{" stmtList "}"
    ;

paramList
    : nonEmptyParamList
    |
    ;

nonEmptyParamList
    : nonEmptyParamList "," param
    | param
    ;

param
    : type IDENTIFIER
    ;

type
    : INT
    | FLOAT
    ;

stmtList
    : stmtList stmt
    |
    ;

stmt
    : var "=" orExpr ";"
    | WHILE "(" orExpr ")" block
    | IF "(" orExpr ")" block elseIfBlocks
    | IF "(" orExpr ")" block elseIfBlocks ELSE block
    | RETURN orExpr ";"
    | RETURN ";"
    | block
    ;

elseIfBlocks
    : elseIfBlocks ELSE IF "(" orExpr ")" block
    |
    ;

var
    : IDENTIFIER
    | IDENTIFIER "[" orExpr "]"
    ;

relOp
    : "<"
    | "<="
    | ">"
    | ">="
    | "=="
    | "!="
    ;

addOp
    : "+"
    | "-"
    ;

mulOp
    : "*"
    | "/"
    ;

orExpr
    : orExpr "||" andExpr
    | andExpr;

andExpr
    : andExpr "&&" relExpr
    | relExpr
    ;

relExpr
    : addExpr relOp addExpr
    | addExpr
    ;

addExpr
    : addExpr addOp mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr mulOp term
    | term
    ;

term
    : var
    | NUM
    | FLOAT_NUM
    | "!" term
    | "(" orExpr ")"
    ;

%%

