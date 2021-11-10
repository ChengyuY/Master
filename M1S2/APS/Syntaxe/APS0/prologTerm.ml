(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_prolog_type typ = 
  match typ with
    Type(typp) -> Printf.printf "%s" (string_of_tprim typp)
  | TypeFunc(typs, typ) -> (
      Printf.printf "typeFunc([";
      print_prolog_types typs;
      Printf.printf "],";
      print_prolog_type typ;
      Printf.printf ")"
    )
and print_prolog_types typs = 
  match typs with
    ASTType(typ) -> print_prolog_type typ
  | ASTTypes(typ, types) -> (
      print_prolog_type typ;
      Printf.printf ",";
      print_prolog_types types;
    )

let print_prolog_arg arg = 
  match arg with
    Argu(id, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ")"
    )

let rec print_prolog_args args = 
  match args with
    ASTArg(arg) -> print_prolog_arg arg
  | ASTArgs(arg, argus) -> (
      print_prolog_arg arg;
      Printf.printf ",";
      print_prolog_args argus;
    )
;;

let rec print_prolog_expr expr =
  match expr with
    ASTNum num -> Printf.printf"num(%d)" num
  | ASTId id -> Printf.printf"id(%s)" id
  | ASTBool(bool) -> Printf.printf "%b" bool
  | ASTBinary(op, expr1, expr2) ->(
    Printf.printf "%s" (string_of_opdeux op);
    Printf.printf "(";
    print_prolog_expr expr1;
    Printf.printf ",";
    print_prolog_expr expr2;
    Printf.printf ")";
  )
  | ASTUnary(op, expr) -> (
    Printf.printf "%s" (string_of_opun op);
    Printf.printf "(";
    print_prolog_expr expr;
    Printf.printf "(";
  )
  | ASTIf(expr1, expr2, expr3) -> (
      Printf.printf "if(";
      print_prolog_expr expr1;
      Printf.printf ",";
      print_prolog_expr expr2;
      Printf.printf ",";
      print_prolog_expr expr3;
      Printf.printf ")";
    )
  | ASTFunc(args, expr) -> (
      Printf.printf "func([";
      print_prolog_args args;
      Printf.printf "],";
      print_prolog_expr expr;
      Printf.printf ")";
    )
  | ASTApp(sexpr, exprs) -> (
    Printf.printf "app(";
    print_prolog_expr sexpr;
    Printf.printf ",[";
    print_prolog_exprs exprs;
    Printf.printf "])"
  )
and print_prolog_exprs exprs =
  match exprs with
    ASTExpr(expr) -> print_prolog_expr expr
  | ASTExprs(expr, sexprs) -> (
      print_prolog_expr expr;
      Printf.printf ",";
      print_prolog_exprs sexprs;
    )

let print_prolog_stat stat = 
  match stat with
    ASTEcho(expr) -> (
      Printf.printf "echo(";
      print_prolog_expr expr;
      Printf.printf ")"
    )

let print_prolog_dec dec = 
  match dec with
    ASTConst(id, typ, expr) -> (
      Printf.printf "const(";
      Printf.printf "%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",";
      print_prolog_expr expr;
      Printf.printf ")";
    )
  | ASTFun(id, typ, args, expr) -> (
      Printf.printf "fun(";  
      Printf.printf "%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",[";
      print_prolog_args args;
      Printf.printf "],";
      print_prolog_expr expr;
      Printf.printf ")";
    )
  | ASTFunRec(id, typ, args, expr) -> (
      Printf.printf "funRec(";
      Printf.printf "%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",[";
      print_prolog_args args;
      Printf.printf "],";
      print_prolog_expr expr;
      Printf.printf ")";
  )

let rec print_prolog_cmds cmd = 
  match cmd with
    ASTStat(stat) -> (
      Printf.printf "stat(";
      print_prolog_stat stat;
      Printf.printf ")";
      )
  | ASTDec(dec, cmds) -> (
      Printf.printf "dec(";
      print_prolog_dec dec;
      Printf.printf "),";
      print_prolog_cmds cmds;
    )
  | ASTStatCmd(stat, cmds) -> (
    Printf.printf "stat(";
    print_prolog_stat stat;
    Printf.printf "),";
    print_prolog_cmds cmds;
  )

let print_prolog_prog prog = 
  match prog with
    ASTProg(cmds) -> (
      Printf.printf "prog([";
      print_prolog_cmds cmds;
      Printf.printf "]).";
    )
;;

let file = open_in Sys.argv.(1)
let _ =
 try
   let lexbuf = Lexing.from_channel file in
   let e = Parser.prog Lexer.token lexbuf in
	    print_prolog_prog e;
	    print_char '\n'
 with Lexer.Eof -> exit 0