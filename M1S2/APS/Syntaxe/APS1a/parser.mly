%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token INT BOOL 
%token TRUE FALSE
%token PLUS MINUS MUL DIV AND OR EQ LT
%token NOT
%token IF
%token ECHO CONST FUN REC
%token LPAR RPAR LCRO RCRO
%token COLON SEMICOLON COMA ARROW STAR 
/*APS1*/
%token VAR PROC SET IFB WHILE CALL
/*APS1a*/
%token VARI ADR

%type <Ast.prog> prog
%type <Ast.cmds> cmds
%type <Ast.stat> stat
%type <Ast.dec> dec
%type <Ast.typ> typ
%type <Ast.typs> types
%type <Ast.arg> arg
%type <Ast.args> args
%type <Ast.expr> expr
%type <Ast.exprs> exprs
/*APS1*/
%type <Ast.block> block
/*APS1a*/
%type <Ast.argp> argp
%type <Ast.argsp> argsp
%type <Ast.exprp> exprp
%type <Ast.exprsp> exprsp


%start prog             /* the entry point */

%%

  prog:
    LCRO cmds RCRO            { ASTProg($2) }
  ;

  cmds:
    stat                      { ASTStat($1) }
  | dec SEMICOLON cmds        { ASTDec($1, $3) }
	| stat SEMICOLON cmds       { ASTStatCmd($1, $3) }
  ;

  stat:
    ECHO expr                 { ASTEcho($2) }
  /*APS1*/
  | SET IDENT expr           {ASTSet($2,$3)}
  | IFB expr block block     {ASTIfb ($2,$3,$4)}
  | WHILE expr block         {ASTWhile($2, $3)}
  | CALL expr exprsp         {ASTCall($2,$3)}
  ;

  dec:
    CONST IDENT typ expr                  { ASTConst($2, $3, $4) }
  | FUN IDENT typ LCRO args RCRO expr     { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT typ LCRO args RCRO expr { ASTFunRec($3,$4,$6,$8) }
  /*APS1*/
	| VAR IDENT typ                         { ASTVar($2,$3) }
  /*APS1a*/
	| PROC IDENT LCRO argsp RCRO block      { ASTProc($2,$4,$6) }
	| PROC REC IDENT LCRO argsp RCRO block  { ASTProcRec($3,$5,$7) }
  ;

  /*APS1*/
  block:
    LCRO cmds RCRO {ASTBlock($2)}
  ;
  
  typ:
    INT                       { Int }
  | BOOL                      { Bool }
  | LPAR types ARROW typ RPAR { ASTArrow($2,$4) }
  ;

  types:
    typ                      { ASTType($1) }
  | typ STAR types           { ASTTypes($1, $3) }
  ;

  arg:
    IDENT COLON typ          { Argu($1, $3) }
  ;

  args:
    arg                      { ASTArg($1) }
  | arg COMA args            { ASTArgs($1, $3) }
  ;

  /*APS1a*/
  argp:
    IDENT COLON typ          { Argup($1, $3) }
  | VARI IDENT COLON typ     { Argupp($2, $4) }
  ;

  argsp:
    argp                     { ASTArgp($1) }
  | argp COMA argsp          { ASTArgsp($1, $3) }
  ;

  expr:
    NUM                       { ASTNum($1) }
  | IDENT                     { ASTId($1) }
  | TRUE                      { ASTBool(true) }
  | FALSE                     { ASTBool(false) }
  | LPAR NOT expr RPAR        { ASTUnary(Ast.Not,$3) }
	| LPAR PLUS expr expr RPAR  { ASTBinary(Ast.Plus, $3, $4) }
	| LPAR MINUS expr expr RPAR { ASTBinary(Ast.Minus, $3, $4) }
	| LPAR MUL expr expr RPAR   { ASTBinary(Ast.Mul, $3, $4) }
	| LPAR DIV expr expr RPAR   { ASTBinary(Ast.Div, $3, $4) }
	| LPAR AND expr expr RPAR   { ASTBinary(Ast.And, $3, $4) }
	| LPAR OR expr expr RPAR    { ASTBinary(Ast.Or, $3, $4) }
	| LPAR EQ expr expr RPAR    { ASTBinary(Ast.Eq, $3, $4) }
	| LPAR LT expr expr RPAR    { ASTBinary(Ast.Lt, $3, $4) }
	| LCRO args RCRO expr       { ASTFunc($2,$4) }
	| LPAR expr exprs RPAR      { ASTApp($2,$3) }
	| LPAR IF expr expr expr RPAR { ASTIf($3,$4,$5) }
  ;

  exprs:
	  expr         { ASTExpr($1) }
	| expr exprs   { ASTExprs($1,$2) }
  ;

  exprp:
    expr                   {Exprp($1)}
  | LPAR ADR expr RPAR    {Exprpp($3)}

  exprsp:
	  exprp        { ASTExprp($1) }
	| exprp exprsp { ASTExprsp($1,$2) }
  ;

