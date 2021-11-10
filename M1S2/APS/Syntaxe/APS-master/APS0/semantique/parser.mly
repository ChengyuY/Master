
/*fichier parser.mly*/

%{
open Ast
%}
%token <int> NUM
%token <string> IDENT
%token PLUS MINUS TIMES DIV LT EQ
%token LPAR RPAR LCRO RCRO SEPARATOR VIRGU ETOILE FLECHE DPOINTS 
%token EOL NOT AND OR 
%token CONST FUN REC ECHO IF
%token BOOL INT
%token TRUE FALSE
%start program             /* the entry point */
%type <Ast.expr> line
%type <Ast.prog> program

%%program:
prog     {$1};


line:
expr                    { $1 }
;
expr:					
| NUM                        { ASTNum($1) }
| IDENT                      { ASTId($1) }
| LPAR PLUS expr expr RPAR   { ASTOPrim(Ast.Add, $3, $4) }
| LPAR MINUS expr expr RPAR  { ASTOPrim(Ast.Sub, $3, $4) }
| LPAR TIMES expr expr RPAR  { ASTOPrim(Ast.Mul, $3, $4) }
| LPAR DIV expr expr RPAR    { ASTOPrim(Ast.Div, $3, $4) }
| LPAR EQ expr expr RPAR    { ASTOPrim(Ast.Eq, $3, $4) }
| LPAR LT expr expr RPAR    { ASTOPrim(Ast.Lt, $3, $4) }
| LPAR IF expr expr expr RPAR {ASTIf($3,$4,$5)}
| LCRO args RCRO expr {ASTFuncExpr($2,$4)}
| LPAR expr exprs RPAR {ASTExprs($2,$3)}

| LPAR NOT expr RPAR     { ASTNot(Ast.Not, $3) }
| LPAR AND expr expr RPAR  { ASTBPrim(Ast.And , $3,$4) }
| LPAR OR expr expr RPAR   { ASTBPrim(Ast.Or, $3,$4) }
| TRUE  { ASTBool(true) }
| FALSE { ASTBool(false) }

;

exprs:
|expr  {Expr($1)}
|expr exprs {Exprs($1,$2)}
;

letype:
|INT {TPRIM(Ast.Int)}
|BOOL {TPRIM(Ast.Bool)}
|LPAR types FLECHE letype RPAR {TypeFunc($2,$4)}
;

types:
|letype   {Type($1)}
|letype ETOILE types {Types($1,$3)}
;



dec:
| CONST IDENT letype expr {Const($2,$3,$4)}
| FUN IDENT letype LCRO args RCRO expr {Fun($2,$3,$5,$7)}
| FUN REC IDENT letype LCRO args RCRO expr {FunRec($3,$4,$6,$8)}
;


arg:
|IDENT DPOINTS letype {ASTArg($1,$3)}
;

args:
|arg {Arg($1)}
|arg VIRGU args {Args($1,$3)}
;

stat:
|ECHO expr {Echo($2)}
;

cmds:
|stat {Stats($1)}
|dec SEPARATOR cmds   {Dec($1,$3)}
|stat SEPARATOR cmds  {Stat($1,$3)}
;

prog:
|LCRO cmds RCRO {Cmds($2)}
;

