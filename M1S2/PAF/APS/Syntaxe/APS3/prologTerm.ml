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


let rec print_prolog_type t = 
  match t with
    ASTStype(st) -> Printf.printf "%s" (string_of_stype st)
  | ASTArrow(typs, typ) -> (
      Printf.printf "arrow";
      Printf.printf "([";
      print_prolog_types typs;
      Printf.printf "],";
      print_prolog_type typ;
      Printf.printf ")"
    )
  | ASTVec(typ) -> (
      Printf.printf "vec";
	    Printf.printf"(";
	    print_prolog_type typ;
	    Printf.printf")";
  )
    
and print_prolog_types typs = 
  match typs with
    ASTType(typ) -> print_prolog_type typ
  | ASTTypes(typ, types) -> (
      print_prolog_type typ;
      Printf.printf ",";
      print_prolog_types types;
    )

and print_prolog_arg arg = 
  match arg with
    Argu(id, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ")"
    )

and print_prolog_args args = 
  match args with
    ASTArg(arg) -> print_prolog_arg arg
  | ASTArgs(arg, argus) -> (
      print_prolog_arg arg;
      Printf.printf ",";
      print_prolog_args argus;
    )

and print_prolog_argp argp =
  match argp with
    Argup(id, typ) -> (
      Printf.printf "(%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ")"
    )
  | Argupp(id, typ) -> (
      Printf.printf "(%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ")"
    )

and print_prolog_argsp argsp = 
  match argsp with
    ASTArgp(argp) -> print_prolog_argp argp
  | ASTArgsp(argp, argspp) -> (
      print_prolog_argp argp;
      Printf.printf ",";
      print_prolog_argsp argspp;
    )    

and print_prolog_expr expr =
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
    Printf.printf ")";
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
  | ASTNth(vec, n) -> (
    Printf.printf"nth(";
	  print_prolog_expr vec;
	  Printf.printf",";
	  print_prolog_expr n;
	  Printf.printf")"
  )
  | ASTLen(vec) -> (
    Printf.printf"len(";
	  print_prolog_expr vec;
	  Printf.printf")"	
  )
  | ASTAlloc(vec) -> (
    Printf.printf"alloc(";
	  print_prolog_expr vec;
	  Printf.printf")"
  )
and print_prolog_exprs exprs =
  match exprs with
    ASTExpr(expr) -> print_prolog_expr expr
  | ASTExprs(expr, sexprs) -> (
      print_prolog_expr expr;
      Printf.printf ",";
      print_prolog_exprs sexprs;
    )

and print_prolog_exprp exprp =
  match exprp with
    Exprp(expr) -> print_prolog_expr expr
  | Exprpp(lval) -> (
      Printf.printf"adr(";
      print_prolog_lval lval;
      Printf.printf")";
    )

and print_prolog_exprsp exprsp =
  match exprsp with
    ASTExprp(exprp) -> print_prolog_exprp exprp
  | ASTExprsp(exprp, sexprsp) -> (
      print_prolog_exprp exprp;
      Printf.printf ",";
      print_prolog_exprsp sexprsp;
    )

and print_prolog_block block =
	match block with
	ASTBlock(cmds) -> (
    Printf.printf "block([";
		print_prolog_cmds cmds;
		Printf.printf "])")

and print_prolog_lval v =
  match v with
    ASTLvalid(ident) -> (
      Printf.printf "id(%s)" ident;
    )
  | ASTLval(lval, expr) -> (
      Printf.printf"nth(";
	    print_prolog_lval lval;
	    Printf.printf ",";
	    print_prolog_expr expr;
	    Printf.printf")"
  )

and print_prolog_stat stat = 
  match stat with
    ASTEcho(expr) -> (
      Printf.printf "echo(";
      print_prolog_expr expr;
      Printf.printf ")"
    )
	|ASTSet(lval,e)-> (
      Printf.printf "set(";
      print_prolog_lval lval; 
      Printf.printf ",";
			print_prolog_expr e;
			Printf.printf ")")				 
	|ASTIfb(e,b0,b1) ->(
      Printf.printf "ifb(";
		  print_prolog_expr e;
		  Printf.printf ",";
			print_prolog_block b0;
			Printf.printf ",";
			print_prolog_block b1;
			Printf.printf ")")
	|ASTWhile(e,b) -> (
      Printf.printf "while(";
			print_prolog_expr e;
			Printf.printf ",";
			print_prolog_block b;
			Printf.printf ")")
	|ASTCall(id,p) -> (
      Printf.printf "call(";
			print_prolog_expr id;
			Printf.printf ",[";
			print_prolog_exprsp p;
			Printf.printf "])")	

and print_prolog_dec dec = 
  match dec with
    ASTConst(id, typ, expr) -> (
      Printf.printf "const(%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",";
      print_prolog_expr expr;
      Printf.printf ")";
    )
  | ASTFun(id, typ, args, expr) -> (
      Printf.printf "fun(%s" id;  
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",[";
      print_prolog_args args;
      Printf.printf "],";
      print_prolog_expr expr;
      Printf.printf ")";
    )
  | ASTFunRec(id, typ, args, expr) -> (
      Printf.printf "funRec(%s" id;
      Printf.printf ",";
      print_prolog_type typ;
      Printf.printf ",[";
      print_prolog_args args;
      Printf.printf "],";
      print_prolog_expr expr;
      Printf.printf ")";
  )
	| ASTVar(id,t) -> (
      Printf.printf "var(%s" id;
			print_prolog_type t;
      Printf.printf ")")
	| ASTProc(id,argsp,block) -> (
      Printf.printf "proc(%s" id;
			Printf.printf "[";
			print_prolog_argsp argsp;
			Printf.printf "],";
			print_prolog_block block;
			Printf.printf ")")
	  |ASTProcRec(id,argsp,block) -> (
      Printf.printf "procRec(%s," id;
			Printf.printf "[";
			print_prolog_argsp argsp;
			Printf.printf "],";
			print_prolog_block block;
      Printf.printf ")")
  | ASTFunb(id,typ,argsp,block) -> (
      Printf.printf "fun(%s," id;
      print_prolog_type typ;
			Printf.printf ",[";
			print_prolog_argsp argsp;
			Printf.printf "],";
			print_prolog_block block;
			Printf.printf ")")
	| ASTFunRecb(id,typ,argsp,block) -> (
      Printf.printf "funRec(%s," id;
      print_prolog_type typ;
			Printf.printf ",[";
			print_prolog_argsp argsp;
			Printf.printf "],";
			print_prolog_block block;
			Printf.printf ")")

and print_prolog_cmds cmd = 
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
  | ASTReturn(ret) -> (
    Printf.printf "return ";
    print_prolog_ret ret;
    Printf.printf(";");
  )
  (*APS3*)
and print_prolog_ret ret =
  match ret with
   ASTRet(e) -> (print_prolog_expr e)

and print_prolog_prog prog = 
  match prog with
    ASTProg(block) -> (
      Printf.printf "prog([";
      print_prolog_block block;
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