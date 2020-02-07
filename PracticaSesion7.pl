% Jorge Villarrubia y Beatriz Herguedas

% Ejercicio 2

% Apartado a

simetrico(void, void).
simetrico(arbol(E1,I1,D1), arbol(E1,I2,D2)):- simetrico(I1,D2), simetrico(I2,D1).

% Apartado b

sumatree(void, 0).
sumatree(arbol(E,I,D), N):- number(E), sumatree(I, N1), sumatree(D, N2), W is N1+N2, N is W+E.

% Apartado c

arbolalista(void, []).
arbolalista(arbol(E,I,D), [E|L]) :- arbolalista(I, L1), arbolalista(D,L2), append(L1,L2,L).

% La siguiente funcion elimina un elemento de cada

elimina_uno_de_cada([],_,[]):-!.
elimina_uno_de_cada([X|Xs],Ac,[X|Z]) :- member(X,Ac), !, elimina_uno_de_cada(Xs,Ac,Z).
elimina_uno_de_cada([X|Xs],Ac,Z) :-  elimina_uno_de_cada(Xs,[X|Ac],Z).

% Si quito uno de cada mientras el mio este
% y acabo dejando la lista vacia el mio estaba >= veces que los de más

elimina_esta(_,[]).
elimina_esta(X,L):- member(X,L), elimina_uno_de_cada(L,[],L1),  elimina_esta(X,L1).

maxVeces(A,X):- arbolalista(A,L), elimina_esta(X,L).

% Ejercicio 3

subterminoListas(S,[S|_]).
subterminoListas(S,[X|Xs]):- atomic(X) -> subterminoListas(S,Xs) ; 
                             X =.. [_|L], append(L,Xs,As), subterminoListas(S,As).

subtermino(S,T):- subterminoListas(S,[T]).


sustituyeListas(_,_,[],[]).

sustituyeListas(T,S,[T|Ts],[S|Ns]):- !, sustituyeListas(T,S,Ts,Ns).
sustituyeListas(T,S,[K|Ts],[K|Ns]):- atomic(K), !, sustituyeListas(T,S,Ts,Ns).
sustituyeListas(T,S,[K|Ts],[K1|Ns]):- K =.. L, sustituyeListas(T,S,L,NE1),
                                      sustituyeListas(T,S,Ts,Ns), K1 =.. NE1.

subst(T,S,E,NE):- sustituyeListas(T,S,[E],[NE]), !.


% Ejercicio 4
torres_hanoi(1,A,B,_,[mov(A,B)]):- !.
torres_hanoi(N,A,B,C,L) :- N1 is N-1, torres_hanoi(N1, A, C, B, L1),
                               torres_hanoi(N1, C, B, A, L2), 
                               append(L1, [mov(A,B)|L2], L).