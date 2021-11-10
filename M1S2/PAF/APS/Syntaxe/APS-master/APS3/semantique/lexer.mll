{
 open Parser
 exception Eof
}

rule token = parse
	[' ' '\t' '\n'] { token lexbuf}
	| ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
	| "add"            { PLUS }
	| "sub"            { MINUS }
	| "mul"            { TIMES }
	| "div"            { DIV }
	| "not"		   { NOT }
	| "and"		   { AND }
	| "or"		   { OR }
	| "eq"		   { EQ }
	| "lt"		   { LT}
	| '('              { LPAR }
	| ')'              { RPAR }
	| '['              { LCRO}
	| ']'              { RCRO}
	| ';'              {SEPARATOR}
	| "->"		   {FLECHE}
	| '*'	           {ETOILE}
	| ':'		   {DPOINTS}
	| "CONST"          {CONST}
	| "FUN"            {FUN}             							
	| "REC"            {REC}
	| "ECHO"           {ECHO}
	| ","		   {VIRGU}
	| "int"		   {INT}
	| "bool"  	   {BOOL}
	| "if"  	   {IF}
	| "true"		{TRUE}
	| "false"		{FALSE}
	(*APS1*)
	| "VAR"		   {VAR}
	| "PROC"	   {PROC}
	| "SET"		   {SET}
	| "WHILE"	   {WHILE}
	| "CALL"	   {CALL}
	| "IF"		   {IF_PROC}
	| "void" 	   {VOID}
	(*APS2*)
	| "nth"         {NTH}
	| "vec"   		{VEC}
	| "len"			{LEN}
	| "alloc"		{ALLOC}
	(*APS3*)
	| "RETURN"		{RETURN}
	| (['a'-'z' 'A'-'Z'])(['a'-'z' 'A'-'Z' '0'-'9'])* as lxm {IDENT lxm }
	| eof              { raise Eof }
