%{
  open Ast_loc
  let mk d s e = { descr = d; loc = Location.make s e }
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token PLUS MINUS TIMES EQ LT
%token FST SND
%token COMMA
%token IF THEN ELSE
%token LPAR RPAR
%token LET LETREC IN
%token FUN ARROW
%token EOF

%nonassoc ELSE
%nonassoc IN
%nonassoc ARROW
%nonassoc COMMA
%left EQ
%nonassoc LT
%left PLUS MINUS
%left TIMES
%nonassoc FST SND

%start program

%type <Ast_loc.expr> program

%%

program:
| e = expr EOF {
    e
}

expr:
| d = descr {
    mk d $startpos $endpos
}

descr:
| e = simple_descr {
    e
}
| e = simple_descr args = simple_descr+ {
    List.fold_left (fun acc e -> 
            App (mk acc $startpos $endpos, mk e $startpos $endpos)
        ) e args
}
| e1 = expr op = binop e2 = expr {
    Binop (op, e1, e2)
}
| op = unop e = expr {
    Unop (op, e)
}
| IF c = expr THEN e1 = expr ELSE e2 = expr {
    If (c, e1, e2)
}
| FUN x = IDENT ARROW body = expr {
    Fun (x, body)
}
| e1 = expr COMMA e2 = expr {
    Pair (e1, e2)
}
| LET x = IDENT EQ e1 = expr IN e2 = expr {
    Let (x, e1, e2)
}
| LETREC f = IDENT EQ e1 = expr IN e2 = expr {
    Letrec (f, e1, e2)
}

%inline 
binop:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| EQ    { Eq }
| LT    { Lt }

%inline 
unop:
| FST  { Fst }
| SND  { Snd }

simple_descr:
| id = IDENT {
    Var id
}
| i = INT {
    Int i
}
| FALSE { 
    Bool false
}
| TRUE { 
    Bool true
}
| LPAR e = descr RPAR {
    e
}
