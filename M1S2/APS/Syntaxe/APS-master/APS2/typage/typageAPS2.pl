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


/*********************APS0********************/

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


/*********************APS2********************/
/*ALLOC*/
typeExpr(G,alloc(E),vec(_)):-typeExpr(G,E,int).
/*NTH*/
typeExpr(G,nth(E1,E2),T):-typeExpr(G,E1,vec(T)),typeExpr(G,E2,int).
/*LEN*/
typeExpr(G,len(E),int):-typeExpr(G,E,vec(_)).
/*********************APS2********************/



/*Instructions*/
/*ECHO*/
typeStat(G,echo(E),void):-typeExpr(G,E,int).
/*Set*/
typeStat(G,set(X,E),void):-typeExpr(G,X,T),typeExpr(G,E,T).
/*IF*/
typeStat(G,ifProc(B,BLOCK1,BLOCK2),void):-typeExpr(G,B,bool),typeBlock(G,BLOCK1,void),typeBlock(G,BLOCK2,void).
/*WHILE*/
typeStat(G,while(B,BLOC),void):-typeExpr(G,B,bool),typeBlock(G,BLOC,void).
/*CALL*/
typeStat(G,call(X,LE),void):-typeExprs(G,LE,LT),typeExpr(G,X,typeFunc(LT,void)).

/*********************APS2********************/
/*SET*/
typeStat(G,setLval(LV,E),void):-typeExpr(G,LV,T),typeExpr(G,E,T).
/*********************APS2********************/


/*Déclaration*/
/*Déclaration const*/
typeDec(G,const(X,T,E),[(X,T)|G]):-typeExpr(G,E,T).
/*Déclaration fonction*/
typeDec(G,fun(X,T,ARGS,E),[(X,typeFunc(TS,T))|G]):-append(G,ARGS,GG),typeExpr(GG,E,T),getTypes(ARGS,TS).
/*Déclaration de fonction récursive*/
typeDec(G,funRec(X,T,ARGS,E),[(X,typeFunc(TS,T))|G]):-append(G,ARGS,G2),append(G2,[(X,typeFunc(TS,T))],GG),typeExpr(GG,E,T),getTypes(ARGS,TS).
/*La déclaration d’une variable*/
typeDec(G,var(X,T),[(X,T)|G]).
/*La déclarations de procédures et procédures récursives*/
/*Déclaration de procédures*/
typeDec(G,proc(X,ARGS,BLOCK),[(X,typeFunc(LT,void))|G]):-append(G,ARGS,GG),typeBlock(GG,BLOCK,void),getTypes(ARGS,LT).
/*Déclaration de procédures récursive*/
typeDec(G,procRec(X,ARGS,BLOCK),[(X,typeFunc(LT,void))|G]):- getTypes(ARGS,LT),append(G,ARGS,G2),append(G2,[(X,typeFunc(LT,void))],GG),typeBlock(GG,BLOCK,void).


/*Suites de commandes*/
/*La suite commence par une déclaration*/
typeCmds(G,[DEC|CMDS],void):-typeDec(G,DEC,G2),typeCmds(G2,CMDS,void).
/*La suite commence par une instruction*/
typeCmds(G,[STAT|CMDS],void):-typeStat(G,STAT,void),typeCmds(G,CMDS,void).
/*suite vide*/
typeCmds(_,[epsilon],void).

/*Programmes*/
typePro(program(CMDS),void):-append(CMDS,[epsilon],L),typeCmds([],L,void).



/*Blocs de commandes*/
typeBlock(G,CMDS,void):-append(CMDS,[epsilon],L),typeCmds(G,L,void).

typeProg(P,true):-typePro(P,void).

main_stdin :-
read(user_input,T),
typeProg(T,R),
print(R).





