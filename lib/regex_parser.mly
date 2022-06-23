%token <string> CLASS
%token <int> NUM
%token <Regex_sketch_ast.unop> UNOP
%token <Regex_sketch_ast.binop> BINOP
%token REPEAT REPEATRANGE REPEATATLEAST LPAREN RPAREN COMMA LBRACK RBRACK EOF QMARK

%start <Regex_sketch_ast.t> sketch_eof
%%

sketch_eof: x = sketch; EOF { x }
num: x = NUM { `Num x } | QMARK { `Hole }
sketch:
  | x = CLASS { `Class x }
  | f = UNOP; LPAREN; x = sketch; RPAREN { `Unop (f, x) }
  | f = BINOP; LPAREN; x = sketch; x1 = sketch; RPAREN { `Binop (f, x, x1) }
  | REPEAT; LPAREN; x = sketch; x1 = num; RPAREN { `Repeat (x, x1) }
  | REPEATATLEAST; LPAREN; x = sketch; x1 = num; RPAREN { `Repeat_at_least (x, x1) }
  | REPEATRANGE; LPAREN; x = sketch; x1 = num; x2 = num; RPAREN { `Repeat_range (x, x1, x2) }
  | LBRACK; xs = separated_nonempty_list(COMMA, sketch); RBRACK { `Hole xs }
