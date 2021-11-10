open Ast;;

type valeur = InN of int 
	| InF of expr * string list * ident list 
	| InFR of valeur 
	| None
and ident = Pair of string * valeur

let print_val value =
	match value with
	InN(n) -> Printf.printf "%d\n" n
	| _ -> failwith "value not printable"

let rec mem_env id list =
	match list with
	Pair(i,v)::tl -> if (String.equal id i) then true else mem_env id tl
	| [] -> false
;;

let rec extract_from_env id list =
	match list with
	Pair(i,v)::tl -> if (String.equal id i) then v else extract_from_env id tl
	| _ -> failwith "extraction variable erreur"
;;

let is_opdeux op = 
  match op with
    Plus -> true
  | Mul -> true
  | Minus -> true
  | Div -> true
  | And -> true
  | Or -> true
  | Eq -> true
  | Lt -> true

let is_opun op =
  match op with
    Not -> true

let get_int value =
	match value with
	|InN(n) -> n
	| _ -> failwith "not inN"

let eval_opdeux op e1 e2 =
	match op with
	 Plus -> InN((get_int e1) + (get_int e2))
	| Mul -> InN((get_int e1) * (get_int e2))
	| Minus -> InN((get_int e1) - (get_int e2))
	| Div -> InN((get_int e1) / (get_int e2))
	| And -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) 
		then failwith "not logical arguments"
		else if (get_int e1) = 0 
			then InN(0) 
			else InN((get_int e2))
	| Or -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not logical arguments"
		else if (get_int e1) = 1 
			then InN(1) 
			else InN((get_int e2))
	| Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
	| Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)

let eval_opun op e =
	match op with
	Not -> if (get_int e) = 0 then InN(1) 
	else if (get_int e) = 1 then InN(0) 
		else failwith "argument not unary"

let get_id arg =
	match arg with
	Argu(id,t) -> id
;;

let rec creer_fermeture args env =
	match args with
	ASTArgs(a1,a2) -> creer_fermeture a2 ([get_id a1]@env)
	|ASTArg(a) -> [get_id a]@env
;;

let rec eval_expr expr env = 
	match expr with
	| ASTNum(n) -> InN(n)
	| ASTId(id) -> if mem_env id env 
		then extract_from_env id env 
		else failwith (id^" not a variable in environment")
	| ASTBool(e) -> if e then InN(1) else InN(0)
	| ASTBinary(opdeux,e1,e2) -> if is_opdeux opdeux 
		then eval_opdeux opdeux (eval_expr e1 env) (eval_expr e2 env) 
		else failwith "operator not binary"
	| ASTUnary(opun,e) -> if is_opun opun then eval_opun opun (eval_expr e env) 
		else failwith "operator not unary"
	| ASTIf(e1,e2,e3) -> 
			if (eval_expr e1 env) = InN(1) 
				then eval_expr e2 env 
				else if (eval_expr e1 env) = InN(0) 
					then (eval_expr e3 env) 
					else failwith "value not a boolean"
	| ASTFunc(args,e_prim) -> InF(e_prim, (creer_fermeture args []),env)
	| ASTApp (expr,exprs) -> 
	match (eval_expr expr env) with
		InF(e_prim,closure,envi) -> let env_fun = (assoc_val closure (get_eval exprs env))@envi in
		eval_expr e_prim env_fun
	| InFR(closure) ->( match closure with
				InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval exprs env))@env in
				eval_expr (ASTApp(e_prim,exprs)) env_fun
			| _ -> failwith "not a recursive function")
	| InN(n) -> InN(n)
	| _ -> failwith "application result not applied yet"

and get_eval exprs env =
	match exprs with
	ASTExprs(e,es) -> (eval_expr e env)::(get_eval es env)
	| ASTExpr(e) -> (eval_expr e env)::[]

and assoc_val fermeture exprs =
	List.map2 (function a -> function e -> Pair(a,e) ) fermeture exprs

let eval_dec dec env=
	match dec with
	| ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env))::env)
	| ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,creer_fermeture args [],env))::env)
	| ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,creer_fermeture args [],env)))::env)

let eval_stat stat env output =
	match stat with
	| ASTEcho(n) -> (eval_expr n env)::output

let rec eval_cmds cmds env output =
	match cmds with
	ASTDec(dec,c) -> eval_cmds c (eval_dec dec env) output
	| ASTStatCmd(stat,c) -> eval_cmds c env (eval_stat stat env output)
	| ASTStat(stat) -> eval_stat stat env output

let rec print_output sortie =
	List.iter (function x -> print_val x) (List.rev sortie) 
;;

let eval_prog prog =
	match prog with
	ASTProg(cmds) -> print_output (eval_cmds cmds [] [])

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0