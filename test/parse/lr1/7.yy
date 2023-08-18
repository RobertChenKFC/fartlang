%token A

%define lr.type canonical-lr

%start a

%%

a
  : A a A
  |
  ;

%%
