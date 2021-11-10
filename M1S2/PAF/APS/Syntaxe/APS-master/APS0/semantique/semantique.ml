open Ast

type valeur = InN of int | InF of expr* (string list)* (string*valeur) list | InFR of string*valeur

let string_of_valeur v = 
	match v with
	 | InN (n)-> "InN ("^(string_of_int n )^")"
	 | InF (expr,args,env)->  "InF "
	| InFR(f,v)-> "InFR "


let get_int v = 
	match v with
	InN(n) -> n
	|_->failwith "probmème get_int"


let not x = 
	match x with
		|0 -> 1
		|1 -> 0 
		|_ -> failwith "probleme argument"

		let rec print_env env = 
			match env with
			|[] -> ()
			|a::l-> let (x,v) = a in 
						match  v with
						| InN(n) -> ( print_string (x^" = "^(string_of_int n)^"\n");			
										print_env l)
						| InF(e,args,env_f) -> ( print_string (x^"  = Inf\n");	print_env l)		
						
						| InFR (nom,valeur )->	( print_string (x^"  = InFR\n");	print_env l)				
						| _-> ( print_string (x^" = "^(string_of_valeur v )^"\n");			
										print_env l)
		
	
	let rec modifier_env env1 env2 = 
		match env2 with 
		|[]-> env1
		|a::l -> (let x,v = a in if List.mem_assoc x env1 then (
												let env3 = 	List.remove_assoc x env1 in 
												let nouveau_env = (x,v)::env3 in 
												modifier_env nouveau_env l
												)else (
												let nouveau_env = (x,v)::env1 in 
												modifier_env nouveau_env l
												)																				
		)
let eval_bprim bp n1 n2 = 
	match bp with
	|And -> 
		if n1 == 0 then 0
 		else if n1 ==1 then n2 
		else failwith "probleme argument"
	|Or ->
		if n1 == 0 then n2 
		else if n1 == 1 then 1 
		else failwith "probleme argument"
	
let eval_prim op n1 n2 = 
	match op with
	|Eq  -> if n1 == n2 then 1 else 0 
	|Lt  -> if n1 < n2 then 1 else 0
	|Add -> n1 + n2
	|Sub -> n1 - n2
	|Mul -> n1 * n2
	|Div -> n1/n2


let rec getValeur id env =
match env with
|[]-> failwith ("variable "^id^" not found")
|a::l-> let (x,v) = a in 
		if String.equal x id then v 
		else getValeur id l
	

let string_of_arg arg = 
match arg with
ASTArg(arg,t)->arg

let rec getArgs args = 
match args with
Arg(arg)->[string_of_arg arg]
|Args(arg,args)->(string_of_arg arg)::(getArgs args)

let get_value v = 
match v with
InN(n)->n
|_->failwith "probmème get_value"

let rec print_env env = 
match env with
|[] -> ()
|a::l-> let (x,v) = a in 
			match  v with
			| InN(n) -> ( print_string (x^" = "^(string_of_int n)^"\n");			
							print_env l)
			| InF(e,args,env_f) -> ( print_string (x^" Inf\n");	print_env l)		
			
			| InFR (nom,valeur )->	( print_string (x^" InFR\n");	print_env l)				
		
let rec modifier_env env1 env2 = 
	match env2 with 
	|[]-> env1
	|a::l -> (let x,v = a in if List.mem_assoc x env1 then (
											let env3 = 	List.remove_assoc x env1 in 
											let nouveau_env = (x,v)::env3 in 
											modifier_env nouveau_env l
											)else (
											let nouveau_env = (x,v)::env1 in 
											modifier_env nouveau_env l
											)																				
	)
			

let rec appli_args args list_value = 
	match args with
	|[]->[]
	|a::[]->[(a,List.hd list_value)]
	|a::l->(a,(List.hd list_value)):: (appli_args l (List.tl list_value))


(*Evaluation de expression*)	
let rec get_list_value exprs env = 
	match exprs with
	|Expr(e)-> [eval_expr e env]
	|Exprs(e,es)->(eval_expr e env)::(get_list_value es env)

(*Application d'une function/function recursive*)
and appli_func f list_v env = 
	match f with
	|InF(e,args,env_f)-> let func_env = appli_args args list_v in 
				let env1 = modifier_env env_f func_env in 
				let env2 = env1@env in 
				(* print_env env2; *)
					eval_expr e env2  
	|InFR(nom,func)-> appli_func func list_v env
	|_->failwith "It's not a function"


		
and eval_expr expr env= 
match expr with 
|ASTNum n -> InN(n)

|ASTOPrim(op,e1,e2)-> 
		let v1 = get_int(eval_expr e1 env ) and v2 = get_int(eval_expr e2 env ) in 
			InN(eval_prim op v1 v2)
|ASTBPrim(bp,e1,e2) -> 
			let v1 = get_int(eval_expr e1 env ) and v2 = get_int(eval_expr e2 env ) in 
				InN(eval_bprim bp v1 v2)
|ASTNot(n,e) ->	let v = get_int(eval_expr e env ) in 
					 InN(not v)
	
|ASTId id -> (getValeur id env) 	
|ASTBool b -> if b == true then InN(1) else InN(0)
|ASTIf(e1,e2,e3)-> let v1 = get_value(eval_expr e1 env) in 
			if v1 == 1 then  eval_expr e2 env
			else if v1 == 0 then eval_expr e3 env
			else failwith "évaluation if erreur"
|ASTFuncExpr(args,e)->let l_args = getArgs args in InF(e,l_args,env)
|ASTExprs(expr,exprs)-> let f = eval_expr expr env in 
			 let l = get_list_value exprs env in 
				appli_func f l env



(*Instruction*)
let eval_stat stat env w =
match stat with
Echo(expr)->
	let res = eval_expr expr env in res::w
	



(*Déclaration d'une fonction*)
let dec_func expr args env = 
	let l_args = getArgs args in 
	InF(expr,l_args, env)

(*Déclaration d'une fonction récursive*)
let dec_func_rec f expr args env = 
	let l_args = getArgs args in 
	let func = InF(expr,l_args, env) in 
		InFR(f,func)
 
(*Déclaration*)
let eval_dec dec env = 
match dec with
|Const(s,t,expr)->
	let v = eval_expr expr env in 
		(s,v)::env
		
|Fun(s,t,args,expr)->
	let v = dec_func expr args env in
		(s,v)::env
	
|FunRec(s,t,args,expr)->
	let v = dec_func_rec s expr args env in
		(s,v)::env
	


(*Suite de commandes*)
let rec eval_cmds cmds env w= 
match cmds with
|Stats(stat)-> 
	eval_stat stat env w;
|Dec(dec,cmds) ->
	let env_r = eval_dec dec env in 
		eval_cmds cmds env_r w;
|Stat(stat,cmds) ->
	let w2 = eval_stat stat env w in
		eval_cmds cmds env w2

(*Programme*)
let evalProgram p = 
match p with
|Cmds(cmds) -> eval_cmds cmds [] []
	
	
(*Print le flux de sortie*)
let rec print_sortie w = 
	match w with
	|[]->()
	|a::l->	let v = get_value a in 
		print_int v;
		print_string " ";
		print_sortie l
		

let texte = open_in Sys.argv.(1)
let _ = 
  try
     let lexbuf = Lexing.from_channel texte in 
     let p = Parser.program Lexer.token lexbuf in 
	let w = evalProgram p in 
		print_sortie w;
		print_newline ()
  with Lexer.Eof -> exit 0
	 




