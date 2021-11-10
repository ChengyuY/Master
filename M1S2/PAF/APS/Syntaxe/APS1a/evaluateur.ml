open Ast;;

type value = 
		InN of int 
	| InF of expr * string list * ident list 
	| InFR of value 
	| InA of int 
	| InP of block * string list * ident list 
	| InPR of value 
	| None
and ident = Pair of string * value

let rec in_env id env =
  match env with
    Pair(i,v)::tl -> if (String.equal id i) then true else in_env id tl
  | [] -> false
;;

let rec get_val_env id env =
  match env with
    Pair(i,v)::tl -> if (String.equal id i) then v else get_val_env id tl
  | _ -> failwith "get error"
;;

let rec get_val_mem address (mem: (int * (value ref)) list) =
  match mem with
    (a,v)::tl -> if (address = a) then v else get_val_mem address tl
  | _ -> failwith "read error"

let indice = ref 0
let alloc mem =
  let res = (!indice, (!indice, ref(InN(-1)))::mem ) in
    indice := (!indice + 1);
    res

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
    InN(n) -> n
  | InF(_,_,_) -> failwith "nothing"
  | _ -> failwith "not N"

let get_ident arg = 
  match arg with
    Argu(id, typ) -> id

let rec make_closure args env = 
  match args with
    ASTArgs(arg, args) -> make_closure args [(get_ident arg)]@env
  | ASTArg(arg) -> [(get_ident arg)]@env

let rec make_closure_argsp argsp =
  match argsp with
    ASTArgp(argp) -> parse_arg argp
  | ASTArgsp(argp,argsp) -> (parse_arg argp)@(make_closure_argsp argsp)
and parse_arg arg =
  match arg with
    Argup(id, typ) -> id::[]
  | Argupp(id, typ) -> id::[]

let eval_opdeux opdeux e1 e2 =
  match opdeux with
    Plus -> InN((get_int e1) + (get_int e2))
  | Mul -> InN((get_int e1) * (get_int e2))
  | Minus -> InN((get_int e1) - (get_int e2))
  | Div -> InN((get_int e1) / (get_int e2))
  | And -> 
    if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) 
    then failwith "invalid op and"
    else if (get_int e1) = 0 then InN(0) else InN((get_int e2))
  | Or -> 
    if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) 
    then failwith "invalid op or"
    else if (get_int e1) = 1 then InN(1) else InN((get_int e2))
  | Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
  | Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)

let eval_opun op e =
  match op with
    Not -> 
    if (get_int e) = 0 then InN(1) 
    else if (get_int e) = 1 then InN(0) 
    else failwith "invalid op not"

let assoc_val closure env =
  List.map2 (function a -> function e -> Pair(a,e) ) closure env

let rec eval_expr expr env mem = 
  match expr with
  | ASTNum(n) -> InN(n)
  | ASTId(id) -> 
    if in_env id env 
    then 
      match get_val_env id env with
        InA(a) -> !(get_val_mem a mem)
      | v -> v
    else failwith (id^" invalide var")
  | ASTBinary(op,e1,e2) -> 
    if is_opdeux op 
    then eval_opdeux op (eval_expr e1 env mem) (eval_expr e2 env mem) 
    else failwith "invalid opdeux"
  | ASTUnary(op,e) -> 
    if is_opun op 
    then eval_opun op (eval_expr e env mem) 
    else failwith "invalid opun"
  | ASTIf(e1,e2,e3) -> 
    if (eval_expr e1 env mem) = InN(1) 
    then eval_expr e2 env mem
    else (eval_expr e3 env mem) 
  | ASTFunc(args,e_prim) -> InF(e_prim, (make_closure args []),env)
  | ASTBool(e) -> if e then InN(1) else InN(0)
  | ASTApp (expr,exprs) -> 
    match (eval_expr expr env mem) with
      InF(e_prim,closure,envi) -> 
      let env_fun = (assoc_val closure (get_eval_exprs exprs env mem))@envi in
      eval_expr e_prim env_fun mem
    | InFR(closure) ->( 
      match closure with
        InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval_exprs exprs env mem))@env in
        eval_expr (ASTApp(e_prim,exprs)) env_fun mem
      | _ -> failwith "invalid recursive")
    | InN(n) -> InN(n)
    | _ -> failwith "invalid application"
and eval_exprp expr env mem =
  match expr with
    Exprp(e) -> eval_expr e env mem
  | Exprpp(e) -> (
    match e with
      ASTId(id) -> (
      if in_env id env  
      then 
        match get_val_env id env with
          InA(a) -> InA(a)
        | v -> v
      else failwith (id^" invalid var")
    )
    | _ -> failwith ("invalid var")
  )

and get_eval_exprs exprs env mem =
  match exprs with
    ASTExprs(e,es) -> (eval_expr e env mem)::(get_eval_exprs es env mem)
  | ASTExpr(e) -> (eval_expr e env mem)::[]

and get_eval_exprsp exprs env mem = 
  match exprs with
    ASTExprsp(e, es) -> (eval_exprp e env mem)::(get_eval_exprsp es env mem)
  | ASTExprp(e) -> (eval_exprp e env mem)::[]

and eval_dec dec env mem =
  match dec with
  | ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env mem))::env,mem)
  | ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,make_closure args [],env))::env,mem)
  | ASTFunRec(id,t,args,expr) -> (Pair(id,InFR(InF(expr,make_closure args [],env)))::env,mem)
  | ASTVar(id,t) -> let (adr, mem1) = alloc(mem) in (Pair(id,(InA(adr)))::env,mem1)
  | ASTProc(id,args,block) -> (Pair(id, InP(block,make_closure_argsp args,env))::env,mem)
  | ASTProcRec(id,args,block) -> (Pair(id, InPR(InP(block,make_closure_argsp args,env)))::env,mem)

and eval_stat stat env mem output =
  match stat with
    ASTEcho(n) -> (mem,(eval_expr n env mem)::output)
  | ASTSet(id,e) -> (
    match get_val_env id env with
      InA(address) -> 
      let v = (get_val_mem address mem) and affectation = eval_expr e env mem in 
      v := affectation;
      (mem,output)
    | _ -> failwith (id^" invalid adr")
    )
	| ASTIfb (e,b1,b2) -> 
		if (eval_expr e env mem) = InN(1) 
    then (eval_block b1 env mem output)
    else (eval_block b2 env mem output)
	| ASTWhile(e,b) -> 
		if (eval_expr e env mem) = InN(0)
    then (mem, output)
		else 
			let (mem1, output1) = (eval_block b env mem output) in
    	eval_stat stat env mem1 output1
  | ASTCall(p,exprs) -> 
    match (eval_expr p env mem) with
      InP(block,closure,envi) -> let env_proc = (assoc_val closure (get_eval_exprsp exprs env mem))@envi in
      eval_block block env_proc mem output
    | InPR(closure) ->( 
      match closure with
        InP(block,f,envi) -> 
        let env_proc = (assoc_val f (get_eval_exprsp exprs env mem))@env in
        eval_block block env_proc mem output
      | _ -> failwith "invalid recursive")
    | _ -> failwith "invalid call"

and eval_cmds cmds env mem output =
  match cmds with
    ASTDec(dec,c) -> let (new_env, mem1) = (eval_dec dec env mem) in eval_cmds c new_env mem1 output
  | ASTStatCmd(stat,c) -> let (mem1, _) = (eval_stat stat env mem output) in eval_cmds c env mem1 output
  | ASTStat(stat) -> eval_stat stat env mem output

and eval_block block env mem output = 
  match block with
    ASTBlock(cmds) -> eval_cmds cmds env mem output

let print_val value =
  match value with
    InN(n) -> Printf.printf "%d\n" n
  | _ -> failwith "invalid value"

let rec print_output output =
  List.iter (function x -> print_val x) (List.rev output) 

let eval_prog prog =
  match prog with
    ASTProg(cmds) -> let (_, out) = (eval_cmds cmds [] [] []) in print_output out

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
		  (eval_prog p)
	with Lexer.Eof -> exit 0