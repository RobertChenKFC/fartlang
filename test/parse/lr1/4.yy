%token a b

%define lr.type canonical-lr

%start S

%%

S
  : A B
  ;

A
  :
  | a A
  ;

B
  :
  | b B
  ;

%%
