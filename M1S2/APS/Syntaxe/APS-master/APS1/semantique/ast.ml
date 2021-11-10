type op = Add | Mul | Sub | Div | Eq | Lt 
type bp =  And  | Or 
type not = Not

type tprim = Int | Bool 
type letype = TPRIM of tprim| TypeFunc of types*letype | VOID
and types = Type of letype | Types of letype*types
type arg = ASTArg of string*letype
type args = Arg of arg |Args of arg*args

type expr = ASTBool of bool |ASTNum of int | ASTId of string | ASTOPrim of op*expr*expr | ASTIf of expr*expr*expr 
		|ASTFuncExpr of args*expr | ASTExprs of expr*exprs |ASTBPrim of bp*expr*expr | ASTNot of not*expr
and exprs = Expr of expr | Exprs of expr*exprs
 

 

type cmds = Stats of stat | Dec of dec*cmds | Stat of stat*cmds
and stat = Echo of expr | Set of string*expr | IfProc of expr*block*block | ASTWhile of expr*block | ASTCall of string*exprs
and block = CmdsBlock of cmds
and dec = Const of string*letype*expr |Fun of string*letype*args*expr |FunRec of string*letype*args*expr
		|Var of string*letype | ASTProc of string*args*block | ASTProcRec of string*args*block
type prog = Cmds of cmds
let string_of_op op =
	match op with
		 Add ->"add"
		|Mul ->"mul"
		|Sub ->"sub"
		|Div ->"div"
		|Eq  ->"eq"
		|Lt  ->"lt"

let string_of_bp bp =
	match bp with
		And ->"and"
		|Or ->"or"
	
	

let op_of_string op = 
	match op with
		"add"->Add
		|"mul"->Mul
		|"sub"->Sub
		|"div"->Div
		|"eq" ->Eq
		|"lt" ->Lt
		|_ -> failwith "op invalide"
		

let string_of_tprim tprim =
	match tprim with
		Int -> "int"
		|Bool -> "bool"






