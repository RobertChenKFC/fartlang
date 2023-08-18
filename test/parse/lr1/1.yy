%token a b

%start S

%%

S
  : A A
  ;

A
  : a A
  | b
  ;

%%
