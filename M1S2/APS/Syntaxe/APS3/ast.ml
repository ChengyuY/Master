(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type opun = Not
type opdeux = Plus | Mul | Minus | Div | Eq | Lt | And | Or 


type stype =
    Int
  | Bool
and typ = 
    ASTStype of stype
  | ASTArrow of typs * typ
  | ASTVec of typ
and typs = ASTType of typ 
  | ASTTypes of typ * typs

and arg = Argu of string * typ

and args = 
    ASTArg of arg 
  | ASTArgs of arg * args

and expr =
    ASTNum of int
  | ASTId of string
  | ASTBool of bool
  | ASTBinary of opdeux * expr * expr
  | ASTUnary of opun * expr
  | ASTApp of expr * exprs
  | ASTIf of expr * expr * expr
  | ASTFunc of args * expr
  | ASTNth of expr * expr
	| ASTLen of expr
	| ASTAlloc of expr 
and exprs =
    ASTExpr of expr 
  | ASTExprs of expr * exprs

and ret = 
    ASTRet of expr

and cmds = 
    ASTStat of stat
  | ASTDec of dec * cmds
  | ASTStatCmd of stat * cmds
  | ASTReturn of ret

and block=
	ASTBlock of cmds

and stat = 
    ASTEcho of expr
	| ASTSet of lval * expr
	| ASTIfb of expr * block * block
	| ASTWhile of expr * block
  | ASTCall of expr * exprsp

and dec = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr
	| ASTVar of string * typ
  | ASTProc of string * argsp * block
  | ASTProcRec of string * argsp * block
  | ASTFunb of string * typ * argsp * block
  | ASTFunRecb of string * typ * argsp * block

and argsp = 
    ASTArgp of argp
  | ASTArgsp of argp * argsp

and argp =
    Argup of string * typ
  | Argupp of string * typ

and exprsp =
    ASTExprp of exprp
  | ASTExprsp of exprp * exprsp

and exprp =
    Exprp of expr
  | Exprpp of lval

and lval = 
    ASTLvalid of string 
  | ASTLval of lval * expr

and prog = 
    ASTProg of block
  
let opdeux_of_string opdeux =
  match opdeux with
    "add" -> Plus
  | "mul" -> Mul
  | "sub" -> Minus
  | "div" -> Div
  | "and" -> And
  | "or" -> Or
  | "eq" -> Eq
  | "lt" -> Lt
  | _ -> failwith "NOT an operator"

let string_of_opdeux opdeux = 
  match opdeux with
    Plus -> "add"
  | Mul -> "mul"
  | Minus -> "sub"
  | Div -> "div"
  | And -> "and"
  | Or -> "or"
  | Eq -> "eq"
  | Lt -> "lt"

let opun_of_string opun =
  match opun with
    "not" -> Not
  | _ -> failwith "not an operator"

let string_of_opun opun =
  match opun with
    Not -> "not"

let string_of_stype stype =
  match stype with
    Int -> "int"
  | Bool -> "bool"
    
