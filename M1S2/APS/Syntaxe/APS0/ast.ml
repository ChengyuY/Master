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

type tprim = Int | Bool

type typ = 
  Type of tprim 
| TypeFunc of typs * typ
and typs = ASTType of typ 
| ASTTypes of typ * typs

type arg = Argu of string * typ

type args = 
  ASTArg of arg 
| ASTArgs of arg * args

type expr =
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

type stat = 
  ASTEcho of expr

type dec = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr

type cmds = 
    ASTStat of stat
  | ASTDec of dec * cmds
  | ASTStatCmd of stat * cmds

type prog = 
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
    
let string_of_tprim tprim =
  match tprim with
    Int -> "int"
  | Bool -> "bool"
