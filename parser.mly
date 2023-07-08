%{
open Term.Source
%}

%token <string> ID
%token LPAREN "("
%token RPAREN ")"
%token LTUPLE "<"
%token RTUPLE ">"
%token LAMBDA "λ"
%token PROJ "π"
%token DOT "."
%token SEMICOLON ";"
%token <int> NUM
%token END
%token EOF

%start <term option> prog
%%

let prog :=
  | EOF; { None }
  | END; p = prog; { p }
  | t = term; line_end; { Some t }

let line_end := END | EOF

let variable :=
  | x = ID; { Var x }

let num :=
  | n = NUM; { n }

let variable_list :=
  | { [] }
  | v = ID; l = variable_list; { v::l }

let atomic :=
  | variable
  | "("; t = term; ")"; { t }
  | "<"; t = term_list; ">"; { Tuple (Array.of_list t) }

let term_list :=
  | { [] }
  | t = term; { [t] }
  | t = term ; ";"; l = term_list ; { t::l }

let non_abstraction :=
  | atomic
  | t = non_abstraction; u = atomic; { App (t, u) }
  | "π"; n = num; u = atomic; { Proj (n,u) }

let abstraction :=
  | "λ"; xs = variable_list; "."; u = term; { Abs (Array.of_list xs, u) }

let term :=
  | non_abstraction
  | abstraction
