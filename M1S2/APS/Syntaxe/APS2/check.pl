assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	typeExpr([],A,T),
	get_type(ARGS,TYPES).

get_typeArgs([],[]).
get_typeArgs([(_,T)],[T]).
get_typeArgs([(_,T)|ARGS],[T|RES]) :-
	get_typeArgs(ARGS,RES).

checkArgs(_,[],[]).
checkArgs(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	typeExpr(C,ARG,ARGTYPE),
	checkArgs(C,ARGS,ARGSTYPE).

get_typeArgsp([],[]).
get_typeArgsp([(_,T)],[T]).
get_typeArgsp([(_,T)|ARGS],[T|RES]) :-
	get_typeArgsp(ARGS,RES).
get_typeArgsp([(varb(_,T))|ARGS],[T|RES]) :-
	get_typeArgsp(ARGS,RES).

checkArgsp(_,[],[]).
checkArgsp(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	typeExprp(C,ARG,ARGTYPE),
	checkArgsp(C,ARGS,ARGSTYPE).

	
/*prog*/
typeProg(C,prog(X),void) :- typeCmds(C,X,void).

/*cmds*/
typeCmds(_,[],void).

typeCmds(C,[stat(X)|Y],void) :-
	typeStat(C,X,void),
	typeCmds(C,Y,void).

/*dec*/
typeCmds(C,[dec(X)|Y],void) :-
	typeDec(C,X,CB),
	typeCmds(CB,Y,void).

/*echo*/
typeStat(C,echo(X),void) :-
	typeExpr(C,X,int).	

/*set*/
typeStat(C,set(ID,E),void) :-
	typeExpr(C,ID,T),
	typeExpr(C,E,T).

/*ifb*/
typeStat(C,ifb(COND,B1,B2),void) :-
	typeExpr(C,COND,bool),
	typeBlock(C,B1,void),
	typeBlock(C,B2,void).

/*while*/
typeStat(C,while(COND,B),void) :-
	typeExpr(C,COND,bool),
	typeBlock(C,B,void).

/*call*/
typeStat(C,call(ID,ARGS),void) :-
	typeExpr(C,ID,arrow(ARGSTYPE,void)),
	checkArgsp(C,ARGS,ARGSTYPE).

/*const*/
typeDec(C,const(X,T,E),[(X,T)|C]) :-
	typeExpr(C,E,T).
	
/*fun*/
typeDec(C,fun(ID,T,ARGS,BODY),CB):-
	append(ARGS,C,CT),
	typeExpr(CT,BODY,T),
	get_typeArgs(ARGS,RES),
	CB=[(ID,arrow(RES,T))|C].
	
/*funRec*/
typeDec(C,funRec(ID,T,ARGS,BODY),CB):-
	get_typeArgs(ARGS,RES),
	append(ARGS,C,CT),
	CTT = [(ID,arrow(RES,T))|CT],
	typeExpr(CTT,BODY,T),
	CB=[(ID,arrow(RES,T))|C].

/*var*/
typeDec(C,var(ID,T),[(ID,T)|C]).

/*proc*/
typeDec(C,proc(ID,ARGS,BLOCK),CB) :-
	append(ARGS,C,CT),
	typeBlock(CT,BLOCK,void),
	get_typeArgsp(ARGS,RES),
	CB=[(ID,arrow(RES,void))|C].

/*proc rec*/
typeDec(C,procRec(ID,ARGS,BLOCK),CB) :-
	get_typeArgsp(ARGS,RES),
	append(ARGS,C,CT),
	CTT = [(ID,arrow(RES,void))|CT],
	typeBlock(CTT,BLOCK,void),
	CB=[(ID,arrow(RES,void))|C].

/*num*/
typeExpr(_,num(X),int) :-
 	integer(X).
 	
/*ident*/
typeExpr(C,id(X),T) :-
	assoc(X,C,T).

/*true*/
typeExpr(_,true,bool).

/*false*/
typeExpr(_,false,bool).

/*if*/
typeExpr(C,if(B,E1,E2),T) :-
	typeExpr(C,B,bool),
	typeExpr(C,E1,T),
	typeExpr(C,E2,T).

/*app*/
typeExpr(C,app(id(F),ARGS),TF) :-
	assoc(F,C,arrow(ARGSTYPE,TF)),
	checkArgs(C,ARGS,ARGSTYPE).
		
typeExpr(C,app(func(ARGSTYPE,BODY),ARGS),TF) :-
	get_typeArgs(ARGSTYPE,RES),
	checkArgs(C,ARGS,RES),
	append(ARGSTYPE,C,CB),
	typeExpr(CB,BODY,TF).
	
typeExpr(C,app(app(E1,E2),ARGS),TR) :-
	get_type(ARGS,LT),
	typeExpr(C,app(E1,E2),arrow(LT,TR)).
				
/*arrow*/
typeExpr(C,func(ARGS,BODY),arrow(_,AR)) :-
	append(ARGS,C,CB),
	typeExpr(CB,BODY,AR).

/*alloc*/
typeExpr(C,alloc(E),vec(_)) :- 
	typeExpr(C,E,int).

/*nth*/
typeExpr(C,nth(E1,E2),T) :-
	typeExpr(C,E1,vec(T)),
	typeExpr(C,E2,int).

/*len*/
typeExpr(C,len(E),int) :- 
	typeExpr(C,E,vec(_)).

/*opérations*/
typeExpr(C,add(E1,E2),int) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).

typeExpr(C,sub(E1,E2),int) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).

typeExpr(C,mul(E1,E2),int) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).

typeExpr(C,div(E1,E2),int) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).

typeExpr(C,and(E1,E2),bool) :-
	typeExpr(C,E1,bool),
	typeExpr(C,E2,bool).

typeExpr(C,or(E1,E2),bool) :-
	typeExpr(C,E1,bool),
	typeExpr(C,E2,bool).

typeExpr(C,eq(E1,E2),bool) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).

typeExpr(C,lt(E1,E2),bool) :-
	typeExpr(C,E1,int),
	typeExpr(C,E2,int).
	
typeExpr(C,not(X),bool) :-
	typeExpr(C,X,bool).

/*exprp*/
typeExprp(C,X,T) :-
	typeExpr(C,X,T).

typeExprp(C,adr(X), T) :-
	typeExpr(C,X,T).

/*block*/
typeBlock(C,block(CMDS),void) :- 
	typeCmds(C,CMDS,void).

main_stdin :-
	read(user_input,T),
	typeProg([],T,R),
	print(R).