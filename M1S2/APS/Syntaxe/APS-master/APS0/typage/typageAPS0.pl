mem(X,[X|_]).
mem(X,[_|Z]):-mem(X,Z).
assoc(K,[(K,V)|_],V).
assoc(K,[_|L],V):-assoc(K,L,V).
append([],L,L).
append([X|Y],L,[X|L2]):-append(Y,L,L2).

typeExprs(_,[],[]).
typeExprs(G,[E|EL],[T|TL]):-typeExpr(G,E,T),typeExprs(G,EL,TL).

getTypes([(_,T)],[T]).
getTypes([(_,T)|ARGS],[T|LT]):-getTypes(ARGS,LT).

/*Expression*/
typeExpr(_,true,bool).
typeExpr(_,false,bool).
typeExpr(_,X,int):-integer(X).
typeExpr(G,not(X),bool):-typeExpr(G,X,bool).
typeExpr(G,and(X,Y),bool):-typeExpr(G,X,bool),typeExpr(G,Y,bool).
typeExpr(G,or(X,Y),bool):-typeExpr(G,X,bool),typeExpr(G,Y,bool).
typeExpr(G,eq(X,Y),bool):-typeExpr(G,X,int),typeExpr(G,Y,int).
typeExpr(G,lt(X,Y),bool):-typeExpr(G,X,int),typeExpr(G,Y,int).
typeExpr(G,add(X,Y),int):-typeExpr(G,X,int),typeExpr(G,Y,int).
typeExpr(G,sub(X,Y),int):-typeExpr(G,X,int),typeExpr(G,Y,int).
typeExpr(G,mul(X,Y),int):-typeExpr(G,X,int),typeExpr(G,Y,int).
typeExpr(G,div(X,Y),int):-typeExpr(G,X,int),typeExpr(G,Y,int).
/*IF*/
typeExpr(G,if(E1,E2,E3),T):-typeExpr(G,E1,bool),typeExpr(G,E2,T),typeExpr(G,E3,T).
typeExpr(G,X,T):-mem((X,T),G).
/*ABS*/
typeExpr(G,astFuncExpr(ARGS,E),typeFunc(TS,T)):-getTypes(ARGS,TS),append(G,ARGS,GG),typeExpr(GG,E,T).
/*Application*/
typeExpr(G,astExprs(F,LE),T):-typeExpr(G,F,typeFunc(LT,T)),typeExprs(G,LE,LT).
/*ECHO*/
typeStat(G,echo(E),void):-typeExpr(G,E,int).

/*Déclaration*/
/*Déclaration const*/
typeDec(G,const(X,T,E),[(X,T)|G]):-typeExpr(G,E,T).

/*Déclaration fonction*/
typeDec(G,fun(X,T,ARGS,E),[(X,typeFunc(TS,T))|G]):-append(G,ARGS,GG),typeExpr(GG,E,T),getTypes(ARGS,TS).

/*Déclaration de fonction récursive*/
typeDec(G,funRec(X,T,ARGS,E),[(X,typeFunc(TS,T))|G]):-append(G,ARGS,G2),append(G2,[(X,typeFunc(TS,T))],GG),typeExpr(GG,E,T),getTypes(ARGS,TS).


/*Suites de commandes*/
/*La suite commence par une déclaration*/
typeCmds(G,[DEC|CMDS],void):-typeDec(G,DEC,G2),typeCmds(G2,CMDS,void).
/*La suite commence par une instruction*/
typeCmds(G,[STAT|CMDS],void):-typeStat(G,STAT,void),typeCmds(G,CMDS,void).
/*suite vide*/
typeCmds(_,[epsilon],void).

/*Programmes*/
typePro(program(CMDS),void):-append(CMDS,[epsilon],L),typeCmds([],L,void).

typeProg(P,true):-typePro(P,void).

main_stdin :-
read(user_input,T),
typeProg(T,R),
print(R).
