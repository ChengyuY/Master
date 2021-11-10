open Ast
let rec print_type t = 
match t with 
TPRIM(tprim)->(
	Printf.printf"%s" (string_of_tprim tprim)
	
)
|TypeFunc(types,letype)->(
	Printf.printf "typeFunc";
	Printf.printf"(";
	Printf.printf"[";
	print_types types;
	Printf.printf"]";
	Printf.printf",";
	print_type letype;
	Printf.printf")"
	
)

and print_types t = 
match t with
Type(letype)->(
	print_type letype
)
|Types(letype,types)->(
	print_type letype;
	Printf.printf",";
	print_types types;

)

let print_arg arg = 
match arg with
ASTArg(arg, t)->(

	Printf.printf"(";
	Printf.printf"%s" arg;
	Printf.printf",";
	print_type t;
	Printf.printf")"

)


let rec print_args args = 
match args with
Arg(arg)->(
	
	print_arg arg;	
	)
|Args(arg,args)->(


	print_arg arg;	
	Printf.printf",";
	print_args args;

)


let rec print_prolog e =
match e with
ASTNum n -> Printf.printf"%d" n
| ASTId x -> Printf.printf"%s" x
| ASTOPrim(op, e1, e2) -> (
	Printf.printf"%s" (string_of_op op);
	Printf.printf"(";
	print_prolog e1;
	Printf.printf",";
	print_prolog e2;
	Printf.printf")"
	)
|ASTBool b -> Printf.printf"%b" b
|ASTIf(e1,e2,e3)-> (
	Printf.printf"if";
	Printf.printf"(";
	print_prolog e1;
	Printf.printf",";
	print_prolog e2;
	Printf.printf",";
	print_prolog e3;
	Printf.printf")"	
	)
|ASTFuncExpr(args,e)->(
	
	Printf.printf"astFuncExpr";
	Printf.printf"(";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_prolog e;
	Printf.printf")"
	)
|ASTExprs(expr,exprs)->(
	Printf.printf"astExprs";
	Printf.printf"(";
	print_prolog expr;
	Printf.printf",";
	Printf.printf"[";
	print_exprs exprs;
	Printf.printf"]";
	Printf.printf")"	
	)

and  print_exprs exprs = 
match exprs with
Expr(e)->(print_prolog e)
|
Exprs(e,es)->(
	print_prolog e;
	Printf.printf",";
	print_exprs es;
)



let print_stat stat = 
match stat with
Echo(expr)->(
	Printf.printf"echo";
	Printf.printf"(";
	print_prolog expr;
	Printf.printf")"

)

let print_dec dec = 
match dec with
Const(s,t,expr)->(
	Printf.printf"const";
	Printf.printf"(";
	Printf.printf"%s" s;
	Printf.printf",";
	print_type t;
	Printf.printf",";
	print_prolog expr;
	Printf.printf")"

	)
|Fun(s,t,args,expr)->(

	Printf.printf"fun";
	Printf.printf"(";
	Printf.printf"%s" s;
	Printf.printf",";
	print_type t;
	Printf.printf",";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_prolog expr;
	Printf.printf")"


)
|FunRec(s,t,args,expr)->(

	Printf.printf"funRec";
	Printf.printf"(";
	Printf.printf"%s" s;
	Printf.printf",";
	print_type t;
	Printf.printf",";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_prolog expr;
	Printf.printf")"
)


let rec print_cmds cmds = 
match cmds with
Stats(stat)->(
	print_stat stat		
)
|Dec(dec,cmds)->(
	print_dec dec;
	Printf.printf",";
	print_cmds cmds;
)
|Stat(stat,cmds)->(
	print_stat stat;
	Printf.printf",";	
	print_cmds cmds;	
)


let print_program p = 
match p with
Cmds(cmds)->(
	Printf.printf"program";
	Printf.printf"(";
	Printf.printf"[";	
	print_cmds cmds;
	Printf.printf"]";	
	Printf.printf")"

	)
let texte = open_in Sys.argv.(1)

let _ =
 try
   let lexbuf = Lexing.from_channel texte in
   let e = Parser.program Lexer.token lexbuf in
	print_program e;
	print_char '\n'
 with Lexer.Eof -> exit 0
