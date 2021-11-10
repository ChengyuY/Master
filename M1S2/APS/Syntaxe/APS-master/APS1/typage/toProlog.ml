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
|VOID->(
	Printf.printf"void" 
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


let rec print_expr e =
match e with
|ASTNum n -> Printf.printf"%d" n
| ASTId x -> Printf.printf"%s" x
| ASTOPrim(op, e1, e2) -> (
	Printf.printf"%s" (string_of_op op);
	Printf.printf"(";
	print_expr e1;
	Printf.printf",";
	print_expr e2;
	Printf.printf")"
	)
|ASTBPrim(bp,e1,e2)->(
	Printf.printf"%s" (string_of_bp bp);
	Printf.printf"(";
	print_expr e1;
	Printf.printf",";
	print_expr e2;
	Printf.printf")"
)
|ASTNot(not,e)->(
	Printf.printf "not";
	Printf.printf"(";
	print_expr e;
	Printf.printf")"
)
|ASTBool b -> Printf.printf"%b" b
|ASTIf(e1,e2,e3)-> (
	Printf.printf"if";
	Printf.printf"(";
	print_expr e1;
	Printf.printf",";
	print_expr e2;
	Printf.printf",";
	print_expr e3;
	Printf.printf")"	
	)
|ASTFuncExpr(args,e)->(
	
	Printf.printf"astFuncExpr";
	Printf.printf"(";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_expr e;
	Printf.printf")"
	)
|ASTExprs(expr,exprs)->(
	Printf.printf"astExprs";
	Printf.printf"(";
	print_expr expr;
	Printf.printf",";
	Printf.printf"[";
	print_exprs exprs;
	Printf.printf"]";
	Printf.printf")"	
	)
and  print_exprs exprs = 
match exprs with
Expr(e)->(print_expr e)
|
Exprs(e,es)->(
	print_expr e;
	Printf.printf",";
	print_exprs es;
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


and print_block block = 
match block with
CmdsBlock(cmds)->(
	Printf.printf"[";
	print_cmds cmds;
	Printf.printf"]";
)

and print_stat stat = 
match stat with
|Echo(expr)->(
	Printf.printf"echo";
	Printf.printf"(";
	print_expr expr;
	Printf.printf")"
)
|Set(var,expr)->(
	Printf.printf"set";
	Printf.printf"(";
	Printf.printf"%s" var;
	Printf.printf",";
	print_expr expr;
	Printf.printf")";
)
|IfProc(expr,block1,block2)->(
	Printf.printf"ifProc";
	Printf.printf"(";
	print_expr expr;
	Printf.printf",";
	print_block block1;
	Printf.printf",";
	print_block block2;
	Printf.printf")";
)
|ASTWhile(expr,block)->(
	Printf.printf"while";
	Printf.printf"(";
	print_expr expr;
	Printf.printf",";
	print_block block;
	Printf.printf")";
) 
|ASTCall(ident,exprs)->(
	Printf.printf"call";
	Printf.printf"(";
	Printf.printf"%s" ident;
	Printf.printf",";
	Printf.printf"[";
	print_exprs exprs;
	Printf.printf"]";
	Printf.printf")";
)

and print_dec dec = 
match dec with
Const(s,t,expr)->(
	Printf.printf"const";
	Printf.printf"(";
	Printf.printf"%s" s;
	Printf.printf",";
	print_type t;
	Printf.printf",";
	print_expr expr;
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
	print_expr expr;
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
	print_expr expr;
	Printf.printf")"
)
|Var(ident,letype)->(
	Printf.printf"var";
	Printf.printf"(";
	Printf.printf"%s" ident;
	Printf.printf",";
	print_type letype;
	Printf.printf")"
)
|ASTProc(ident,args,block)->(
	Printf.printf"proc";
	Printf.printf"(";
	Printf.printf"%s" ident;
	Printf.printf",";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_block block;
	Printf.printf")"
) 
|ASTProcRec(ident,args,block)->(
	Printf.printf"procRec";
	Printf.printf"(";
	Printf.printf"%s" ident;
	Printf.printf",";
	Printf.printf"[";
	print_args args;
	Printf.printf"]";
	Printf.printf",";
	print_block block;
	Printf.printf")"
)




let print_program p = 
match p with
Cmds(cmds)->(
	Printf.printf"program";
	Printf.printf"(";
	Printf.printf"[";	
	print_cmds cmds;
	Printf.printf"]";	
	Printf.printf")."

	)

(*"aps1/prog100.aps"*)
let texte = open_in Sys.argv.(1)

let _ =
 try
   let lexbuf = Lexing.from_channel texte in
   let e = Parser.program Lexer.token lexbuf in
	print_program e;
	print_char '\n'
 with Lexer.Eof -> exit 0
