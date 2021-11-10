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

type typ = 
    Int
  | Bool
  | ASTArrow of typs * typ
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
and exprs =
    ASTExpr of expr 
  | ASTExprs of expr * exprs

and cmds = 
    ASTStat of stat
  | ASTDec of dec * cmds
  | ASTStatCmd of stat * cmds

(*APS1*)
and block=
	ASTBlock of cmds

and stat = 
  ASTEcho of expr
  (*APS1*)
	| ASTSet of string * expr
	| ASTIfb of expr * block * block
	| ASTWhile of expr * block
  | ASTCall of expr * exprsp

and dec = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr
  (*APS1*)
	| ASTVar of string * typ
  (*APS1a*)
  | ASTProc of string * argsp * block
  | ASTProcRec of string * argsp * block

(*APS1a*)  
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
  | Exprpp of expr

and prog = 
  ASTProg of cmds
  
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
    
