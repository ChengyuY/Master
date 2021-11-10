open Ast

type valeur = Vide | InN of int | InF of expr* (string list)* (string*valeur) list 
| InFR of string*valeur | InA of int 
| InP of block *  (string list)* (string*valeur) list
| InPR of string*valeur

let adresse = ref 0

let string_of_valeur v = 
	match v with
	 Vide -> "Vide  "
	 | InN (n)-> "InN ("^(string_of_int n )^")"
	 | InF (expr,args,env)->  "InF "
	| InFR(f,v)-> "InFR "
	| InA (a)->   "InA ("^(string_of_int a )^")"
	| InP (bloc, args, env)-> "InP "
	| InPR (p,v) ->   "InPR "


let alloc memoire  = 
	if not (List.mem_assoc !adresse memoire) then 
		(!adresse,-1)::memoire   (* -1 représente any *)
	else failwith "adresse memoire existe deja "

let get_int v = 
	match v with
	InN(n) -> n
	|_->failwith "probmème get_int"

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

		
let modifer_mem memoire a v =
	let value = get_int v in 
	if List.mem_assoc a memoire then 
		let nouveau = List.remove_assoc a memoire in 
			(a,value)::nouveau
	else failwith "adresse not found in memoire"


let not x = 
	match x with
		|0 -> 1
		|1 -> 0 
		|_ -> failwith "probleme argument"

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

let rec getValeur id env memoire=
match env with
|[]-> failwith ("variable "^id^" not found")
|a::l-> let (x,v) = a in 
		if String.equal x id then v 
		else getValeur id l memoire
	
let string_of_arg arg = 
match arg with
ASTArg(arg,t)->arg

let rec getArgs args = 
match args with
Arg(arg)->[string_of_arg arg]
|Args(arg,args)->(string_of_arg arg)::(getArgs args)


let rec init_list_value args=
	match args with
	|[] -> []
	|a::l -> (a,Vide)::(init_list_value l)

(*Déclaration d'une fonction*)
let dec_func expr args = 
	let l_args = getArgs args in 
	InF(expr,l_args, (init_list_value l_args))

(*Déclaration d'une fonction récursive*)
let dec_func_rec f expr args = 
	let l_args = getArgs args in 
	let func = InF(expr,l_args, (init_list_value l_args)) in 
		InFR(f,func)

(*Déclaration d'une procédure*)
let dec_proc id args bloc = 
	let l_args = getArgs args in
	InP(bloc, l_args, (init_list_value l_args))

(*Déclaration d'une procédure récursive*)
let dec_proc_rec id args bloc = 
	let l_args = getArgs args in 
	let proc = InP(bloc,l_args,(init_list_value l_args)) in 
		InPR(id, proc)


let rec appli_args args list_value = 
	match args with
	|[]->[]
	|a::[]->[(a,List.hd list_value)]
	|a::l->(a,(List.hd list_value)):: (appli_args l (List.tl list_value))


(*Evaluation de expression*)	
let rec get_list_value exprs env memoire = 
	match exprs with
	|Expr(e)-> [eval_expr e env memoire]
	|Exprs(e,es)->(eval_expr e env memoire)::(get_list_value es env memoire) 

(*Application d'une function/function recursive*)
and appli_func f list_v env memoire = 
	match f with
	|InF(e,args,env_f)-> let func_env = appli_args args list_v in 
					let env1 = modifier_env env_f func_env in 
					let env2 = env1@env in 
					(* print_env env2; *)
						eval_expr e env2 memoire 
					
	|InFR(nom,func)-> appli_func func list_v env memoire
	|_->failwith "It's not a function"




and eval_expr expr env memoire = 
match expr with 
|ASTBool b -> if b == true then InN(1) else InN(0)
|ASTNum n -> InN(n)
|ASTId id -> let v = getValeur id env memoire in 
		 (	match v with
			|InA(n)-> let value = List.assoc n memoire in InN(value)   (*ID1*)
			|_ -> v 	(*ID2*)    
		 )
|ASTOPrim(op,e1,e2)-> 
		let v1 = get_int(eval_expr e1 env memoire) and v2 = get_int(eval_expr e2 env memoire) in 
			InN(eval_prim op v1 v2)
|ASTIf(e1,e2,e3)-> let v1 = get_int(eval_expr e1 env memoire) in 
			if v1 == 1 then  eval_expr e2 env memoire
			else if v1 == 0 then eval_expr e3 env memoire
			else failwith "évaluation if erreur"

|ASTFuncExpr(args,e)->let l_args = getArgs args in InF(e,l_args,env)
|ASTExprs(expr,exprs)-> let f = eval_expr expr env memoire in 
			 let l = get_list_value exprs env memoire in 
				appli_func f l env memoire
|ASTBPrim(bp,e1,e2) -> 
		let v1 = get_int(eval_expr e1 env memoire) and v2 = get_int(eval_expr e2 env memoire) in 
			InN(eval_bprim bp v1 v2)
|ASTNot(n,e) ->	let v = get_int(eval_expr e env memoire) in 
		 		InN(not v)

	
(*Déclaration*)
let eval_dec dec env memoire= 
match dec with
|Const(s,t,expr)->
	let v = eval_expr expr env memoire in 
		(s,v)::env , memoire
		
|Fun(s,t,args,expr)->
	let v = dec_func expr args in
		(s,v)::env , memoire
	
|FunRec(s,t,args,expr)->
	let v = dec_func_rec s expr args in
		(s,v)::env , memoire

|Var(id,letype) -> adresse := !adresse + 1 ; 
					let m = alloc memoire  in  
					( id,InA(!adresse) )::env , m 

|ASTProc(id,args,bloc)-> 
	let v = dec_proc id args bloc in 
		(id,v)::env, memoire

 
|ASTProcRec(id,args,bloc)->	
	let v = dec_proc_rec id args bloc in 
		(id,v)::env, memoire


(*Suite de commandes*)
let  rec eval_cmds cmds env memoire w= 
match cmds with
Stats(stat)-> 
	eval_stat stat env memoire w
|Dec(dec,cmds) ->
	let env_r, mem_r = eval_dec dec env memoire in 
		eval_cmds cmds env_r mem_r w
|Stat(stat,cmds) ->
	let mem2, w2 = eval_stat stat env memoire w in
		eval_cmds cmds env mem2 w2

(*Instruction*)
and eval_stat stat env memoire w =
	match stat with
	Echo(expr)->
		let res = eval_expr expr env memoire in memoire,res::w
	
	|Set (id,expr) -> (let v = eval_expr expr env memoire in 
					let v2 =  getValeur id env memoire  in 
					match v2 with
					| InA(a) -> let mem = modifer_mem memoire a v in mem, w 
					| _ -> failwith "set erreur"
					
	)
	|IfProc(b ,bloc1,bloc2)-> let v = get_int(eval_expr b env memoire) in 
							if v == 1 then eval_bloc bloc1 env memoire w 
							else if v == 0 then eval_bloc bloc2 env memoire w 
							else failwith "évaluation if erreur"
	
	|ASTWhile(b, bloc)-> let v = get_int(eval_expr b env memoire) in 
						if v == 0 then memoire,w 
						else if v == 1 then 
						let res_mem, res_w = eval_bloc bloc env memoire w in
						eval_stat stat env res_mem res_w 
						else failwith "évaluation condition while error" 
						
	
	|ASTCall(idproc, exprs)-> let p = getValeur idproc env memoire in 			
						let l_v = get_list_value exprs env memoire in 
						appli_proc p l_v env memoire w 



(* Bloc et suite de commandes*)
and eval_bloc bk env memoire w = 
match bk with
CmdsBlock(cmds)->
	let res_memoire, res_w = eval_cmds cmds env memoire w in res_memoire, res_w



(*Call d'une procédure/procédure recursive*)
and appli_proc p list_v env memoire w=
	match p with 
	|InP(bk,args, args_values)-> let proc_env = appli_args args list_v in 
											eval_bloc bk (proc_env@env) memoire w 
	|InPR(id, proc)-> appli_proc proc list_v env memoire w
	|_-> failwith "invalide id "

(*Programme*)
let evalProgram p = 
match p with
|Cmds(cmds) -> eval_cmds cmds [] [] []
	
	
(*Print le flux de sortie*)
let rec print_sortie w = 
	match w with
	|[]->()
	|a::l->	match  a with
			InN(v) -> print_sortie l;
					  print_int v;
					  print_string " "
					 
			|_->failwith "flux de sortie erreur"
	
let texte = open_in Sys.argv.(1)
let _ = 
  try
     let lexbuf = Lexing.from_channel texte in 
     let p = Parser.program Lexer.token lexbuf in 
	let mem, w = evalProgram p in 
		print_sortie w;
		print_newline ()
  with Lexer.Eof -> exit 0
	 




