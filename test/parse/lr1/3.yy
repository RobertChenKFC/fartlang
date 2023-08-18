%token add mul lParen rParen identifier

%define lr.type canonical-lr

%start E

%%

E
  : E add T
  | T
  ;

T
  : T mul F
  | F
  ;

F
  : identifier
  | lParen E rParen
  ;

%%
