assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	typeExpr([],A,T),
	get_type(ARGS,TYPES).

get_typeArgs([],[]).
get_typeArgs([(_,T)|ARGS],[T|RES]) :-
	get_typeArgs(ARGS,RES).
		
checkArgs(_,[],[]).
checkArgs(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	typeExpr(C,ARG,ARGTYPE),
	checkArgs(C,ARGS,ARGSTYPE).
	
/*prog*/
typeProg(C,prog(X),void) :- typeCmds(C,X,void).

/*end()*/
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

/*const*/
typeDec(C,const(X,T,E),[(X,T)|C]) :-
	typeExpr(C,E,T).
	
/*Fun*/
typeDec(C,fun(ID,T,ARGS,BODY),CB):-
	append(ARGS,C,CT),
	typeExpr(CT,BODY,T),
	get_typeArgs(ARGS,RES),
	CB=[(ID,typeFunc(RES,T))|C].
	
/*funRec*/
typeDec(C,funRec(ID,T,ARGS,BODY),CB):-
	get_typeArgs(ARGS,RES),
	append(ARGS,C,CT),
	CTT = [(ID,typeFunc(RES,T))|CT],
	typeExpr(CTT,BODY,T),
	CB=[(ID,typeFunc(RES,T))|C].

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
typeExpr(C,if(E1,E2,E3),T) :-
	typeExpr(C,E1,bool),
	typeExpr(C,E2,T),
	typeExpr(C,E3,T).

/*app*/
typeExpr(C,app(id(F),ARGS),TF) :-
	assoc(F,C,typeFunc(ARGSTYPE,TF)),
	checkArgs(C,ARGS,ARGSTYPE).

typeExpr(C,app(func(ARGSTYPE,BODY),ARGS),TF) :-
	get_typeArgs(ARGSTYPE,RES),
	checkArgs(C,ARGS,RES),
	append(ARGSTYPE,C,CB),
	typeExpr(CB,BODY,TF).

typeExpr(C,app(app(X,Y),ARGS),TR) :-
	get_type(ARGS,LT),
	typeExpr(C,app(X,Y),typeFunc(LT,TR)).

/*abs*/
typeExpr(C,func(ARGS,BODY),typeFunc(_,TF)) :-
	append(ARGS,C,CB),
	typeExpr(CB,BODY,TF).

/*opérations*/
typeExpr(C,add(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,sub(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,mul(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,div(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,and(X,Y),bool) :-
	typeExpr(C,X,bool),
	typeExpr(C,Y,bool).

typeExpr(C,or(X,Y),bool) :-
	typeExpr(C,X,bool),
	typeExpr(C,Y,bool).

typeExpr(C,eq(X,Y),bool) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,lt(X,Y),bool) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).
	
typeExpr(C,not(X),bool) :-
	typeExpr(C,X,bool).

main_stdin :-
	read(user_input,T),
	typeProg([],T,R),
	print(R).