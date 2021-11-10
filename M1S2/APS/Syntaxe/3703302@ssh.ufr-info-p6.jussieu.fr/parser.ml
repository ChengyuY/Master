type token =
  | NUM of (int)
  | IDENT of (string)
  | INT
  | BOOL
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | EQ
  | LT
  | NOT
  | IF
  | ECHO
  | CONST
  | FUN
  | REC
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | COLON
  | SEMICOLON
  | COMA
  | ARROW
  | STAR
  | VAR
  | PROC
  | SET
  | IFB
  | WHILE
  | CALL
  | VARI
  | ADR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* ========================================================================== *)
(* == U/m/i/4I -- J 2016/2017                          == *)
(* == S/F/m/i/M -- J 2020/2021                     == *)
(* == A d p e s©m                                == *)
(* ========================================================================== *)
(* == S-e S M                                              == *)
(* == F: p.m                                                  == *)
(* == A s                                                   == *)
(* ========================================================================== *)

open Ast

# 56 "parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* BOOL *);
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* MUL *);
  266 (* DIV *);
  267 (* AND *);
  268 (* OR *);
  269 (* EQ *);
  270 (* LT *);
  271 (* NOT *);
  272 (* IF *);
  273 (* ECHO *);
  274 (* CONST *);
  275 (* FUN *);
  276 (* REC *);
  277 (* LPAR *);
  278 (* RPAR *);
  279 (* LCRO *);
  280 (* RCRO *);
  281 (* COLON *);
  282 (* SEMICOLON *);
  283 (* COMA *);
  284 (* ARROW *);
  285 (* STAR *);
  286 (* VAR *);
  287 (* PROC *);
  288 (* SET *);
  289 (* IFB *);
  290 (* WHILE *);
  291 (* CALL *);
  292 (* VARI *);
  293 (* ADR *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\011\000\
\005\000\005\000\005\000\006\000\006\000\007\000\008\000\008\000\
\012\000\012\000\013\000\013\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\010\000\010\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\003\000\004\000\003\000\
\003\000\004\000\007\000\008\000\003\000\006\000\007\000\003\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\003\000\004\000\001\000\003\000\001\000\001\000\001\000\001\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\004\000\004\000\006\000\001\000\002\000\001\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\030\000\031\000\032\000\000\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\018\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\006\000\000\000\000\000\008\000\000\000\
\047\000\000\000\009\000\004\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\046\000\043\000\022\000\024\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\000\000\021\000\000\000\000\000\
\000\000\025\000\000\000\028\000\014\000\000\000\048\000\044\000\
\019\000\011\000\000\000\026\000\015\000\012\000"

let yydgoto = "\002\000\
\004\000\014\000\015\000\016\000\085\000\086\000\049\000\050\000\
\047\000\081\000\062\000\092\000\093\000\066\000\067\000"

let yysindex = "\010\000\
\250\254\000\000\145\255\000\000\057\255\019\255\000\255\024\255\
\003\255\025\255\057\255\057\255\057\255\022\255\026\255\027\255\
\000\000\000\000\000\000\000\000\144\255\046\255\000\000\009\255\
\009\255\048\255\009\255\028\255\052\255\057\255\032\255\032\255\
\081\255\000\000\145\255\145\255\057\255\057\255\057\255\057\255\
\057\255\057\255\057\255\057\255\057\255\057\255\057\255\031\255\
\030\255\036\255\000\000\000\000\009\255\057\255\041\255\009\255\
\000\000\008\255\053\255\000\000\145\255\032\255\000\000\111\255\
\000\000\081\255\000\000\000\000\000\000\057\255\057\255\057\255\
\057\255\057\255\057\255\057\255\057\255\055\255\057\255\057\255\
\059\255\009\255\046\255\057\255\056\255\060\255\000\000\046\255\
\061\255\064\255\089\255\066\255\070\255\008\255\071\255\000\000\
\057\255\000\000\074\255\075\255\076\255\077\255\078\255\079\255\
\083\255\085\255\000\000\057\255\000\000\000\000\000\000\000\000\
\000\000\009\255\009\255\084\255\046\255\009\255\090\255\008\255\
\032\255\086\255\000\000\092\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\106\255\000\000\107\255\057\255\
\109\255\000\000\009\255\000\000\000\000\032\255\000\000\000\000\
\000\000\000\000\057\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\112\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\113\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\254\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\116\255\
\000\000\000\000\000\000\000\000\115\255\000\000\000\000\000\000\
\000\000\000\000\000\000\117\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\224\255\000\000\000\000\247\255\021\000\000\000\182\255\
\251\255\059\000\225\255\000\000\181\255\000\000\074\000"

let yytablesize = 180
let yytable = "\023\000\
\063\000\025\000\068\000\069\000\028\000\031\000\032\000\033\000\
\112\000\090\000\001\000\051\000\052\000\116\000\054\000\055\000\
\003\000\057\000\122\000\026\000\024\000\049\000\029\000\049\000\
\060\000\027\000\030\000\065\000\095\000\053\000\096\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\137\000\091\000\140\000\034\000\089\000\048\000\
\087\000\056\000\058\000\035\000\036\000\059\000\061\000\082\000\
\083\000\017\000\018\000\084\000\065\000\019\000\020\000\088\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\111\000\108\000\080\000\094\000\107\000\021\000\113\000\022\000\
\110\000\017\000\018\000\117\000\114\000\019\000\020\000\115\000\
\118\000\141\000\119\000\124\000\120\000\121\000\123\000\125\000\
\126\000\127\000\128\000\129\000\130\000\064\000\133\000\022\000\
\131\000\135\000\132\000\136\000\138\000\142\000\149\000\017\000\
\018\000\143\000\139\000\019\000\020\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\144\000\
\145\000\148\000\146\000\021\000\147\000\022\000\134\000\002\000\
\023\000\045\000\109\000\098\000\027\000\150\000\020\000\000\000\
\017\000\018\000\000\000\097\000\019\000\020\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\000\000\005\000\006\000\007\000\021\000\000\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\009\000\
\010\000\011\000\012\000\013\000"

let yycheck = "\005\000\
\032\000\002\001\035\000\036\000\002\001\011\000\012\000\013\000\
\083\000\002\001\001\000\003\001\004\001\088\000\024\000\025\000\
\023\001\027\000\094\000\020\001\002\001\024\001\020\001\026\001\
\030\000\002\001\002\001\033\000\061\000\021\001\062\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\117\000\036\001\120\000\024\001\056\000\002\001\
\054\000\002\001\023\001\026\001\026\001\002\001\023\001\025\001\
\027\001\001\001\002\001\024\001\066\000\005\001\006\001\023\001\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\082\000\079\000\080\000\023\001\022\001\021\001\084\000\023\001\
\022\001\001\001\002\001\023\001\029\001\005\001\006\001\028\001\
\025\001\121\000\002\001\097\000\027\001\024\001\024\001\022\001\
\022\001\022\001\022\001\022\001\022\001\021\001\108\000\023\001\
\022\001\115\000\022\001\024\001\118\000\024\001\142\000\001\001\
\002\001\022\001\025\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\022\001\
\022\001\139\000\136\000\021\001\024\001\023\001\114\000\024\001\
\024\001\022\001\080\000\066\000\024\001\147\000\028\001\255\255\
\001\001\002\001\255\255\037\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\017\001\018\001\019\001\021\001\255\255\023\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\031\001\
\032\001\033\001\034\001\035\001"

let yynames_const = "\
  INT\000\
  BOOL\000\
  TRUE\000\
  FALSE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  NOT\000\
  IF\000\
  ECHO\000\
  CONST\000\
  FUN\000\
  REC\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  COLON\000\
  SEMICOLON\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  VAR\000\
  PROC\000\
  SET\000\
  IFB\000\
  WHILE\000\
  CALL\000\
  VARI\000\
  ADR\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 55 "parser.mly"
                              ( ASTProg(_2) )
# 290 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 59 "parser.mly"
                              ( ASTStat(_1) )
# 297 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 60 "parser.mly"
                              ( ASTDec(_1, _3) )
# 305 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 61 "parser.mly"
                             ( ASTStatCmd(_1, _3) )
# 313 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( ASTEcho(_2) )
# 320 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                             (ASTSet(_2,_3))
# 328 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 68 "parser.mly"
                             (ASTIfb (_2,_3,_4))
# 337 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                             (ASTWhile(_2, _3))
# 345 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprsp) in
    Obj.repr(
# 70 "parser.mly"
                             (ASTCall(_2,_3))
# 353 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                                          ( ASTConst(_2, _3, _4) )
# 362 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                                          ( ASTFun(_2, _3, _5, _7) )
# 372 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                                          ( ASTFunRec(_3,_4,_6,_8) )
# 382 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 78 "parser.mly"
                                         ( ASTVar(_2,_3) )
# 390 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.argsp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 80 "parser.mly"
                                         ( ASTProc(_2,_4,_6) )
# 399 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.argsp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 81 "parser.mly"
                                         ( ASTProcRec(_3,_5,_7) )
# 408 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 86 "parser.mly"
                   (ASTBlock(_2))
# 415 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                              ( Int )
# 421 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                              ( Bool )
# 427 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 92 "parser.mly"
                              ( ASTArrow(_2,_4) )
# 435 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 96 "parser.mly"
                             ( ASTType(_1) )
# 442 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typs) in
    Obj.repr(
# 97 "parser.mly"
                             ( ASTTypes(_1, _3) )
# 450 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 101 "parser.mly"
                             ( Argu(_1, _3) )
# 458 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 105 "parser.mly"
                             ( ASTArg(_1) )
# 465 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 106 "parser.mly"
                             ( ASTArgs(_1, _3) )
# 473 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 111 "parser.mly"
                             ( Argup(_1, _3) )
# 481 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 112 "parser.mly"
                             ( Argupp(_2, _4) )
# 489 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp) in
    Obj.repr(
# 116 "parser.mly"
                             ( ASTArgp(_1) )
# 496 "parser.ml"
               : Ast.argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.argsp) in
    Obj.repr(
# 117 "parser.mly"
                             ( ASTArgsp(_1, _3) )
# 504 "parser.ml"
               : Ast.argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 121 "parser.mly"
                              ( ASTNum(_1) )
# 511 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                              ( ASTId(_1) )
# 518 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                              ( ASTBool(true) )
# 524 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                              ( ASTBool(false) )
# 530 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 125 "parser.mly"
                              ( ASTUnary(Ast.Not,_3) )
# 537 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 126 "parser.mly"
                             ( ASTBinary(Ast.Plus, _3, _4) )
# 545 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 127 "parser.mly"
                             ( ASTBinary(Ast.Minus, _3, _4) )
# 553 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 128 "parser.mly"
                             ( ASTBinary(Ast.Mul, _3, _4) )
# 561 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 129 "parser.mly"
                             ( ASTBinary(Ast.Div, _3, _4) )
# 569 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 130 "parser.mly"
                             ( ASTBinary(Ast.And, _3, _4) )
# 577 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 131 "parser.mly"
                             ( ASTBinary(Ast.Or, _3, _4) )
# 585 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 132 "parser.mly"
                             ( ASTBinary(Ast.Eq, _3, _4) )
# 593 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 133 "parser.mly"
                             ( ASTBinary(Ast.Lt, _3, _4) )
# 601 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 134 "parser.mly"
                             ( ASTFunc(_2,_4) )
# 609 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 135 "parser.mly"
                             ( ASTApp(_2,_3) )
# 617 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 136 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 626 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 140 "parser.mly"
                ( ASTExpr(_1) )
# 633 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 141 "parser.mly"
                ( ASTExprs(_1,_2) )
# 641 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 145 "parser.mly"
                           (Exprp(_1))
# 648 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 146 "parser.mly"
                          (Exprpp(_3))
# 655 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp) in
    Obj.repr(
# 149 "parser.mly"
                ( ASTExprp(_1) )
# 662 "parser.ml"
               : Ast.exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprsp) in
    Obj.repr(
# 150 "parser.mly"
                ( ASTExprsp(_1,_2) )
# 670 "parser.ml"
               : Ast.exprsp))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
