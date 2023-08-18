%token a b d

%define lr.type canonical-lr

%start S

%%

S
  : A B
  ;

A
  : a A b
  | a
  ;

B
  : d
  ;

%%
