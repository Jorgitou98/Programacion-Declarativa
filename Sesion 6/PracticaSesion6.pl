% Jorge Villarrubia Elvira

% Ejercicio 1

% profesion(X,Y) sii X la profesion de X es Y
% quizas_tenga(X,Y) sii la profesion X quizas tenga el instrumento Y


% Para la primera consulta profesion(X,Y), quizas_tenga(Y,taladro).
% No hay más que una solución X = roberto, Y = carpintero.
% El primer predicado es un generador de todas las personas con su profesion
% Y el segundo se queda con la las personas por cuya profesión tiene un taladro


% sospechoso(X,Y,Z) sii X es de matar a Y debido al razonamiento Z

% La segunda consulta  sospechoso(X,Y,relacion_sentimental).
% de nuevo tiene una sola respuesta X = alfonso, Y = filomena.
% Es la unica pareja de sospechosos (filomena fue asesinada con algo)
% que cumple el predicado tuvo_relacion_sentimental


% asesinado_con(X,Y) sii X fue asesinado con el instrumento Y

% La tercera consulta asesinado_con(X,Y), sospechoso(Z,Y,_). devuelve false.
% Con razón devuelve false. Esta intentando buscar un objeto con el que 
% fue asesinado alguien (variable Y de asesinado_con) que a su vez tenga
% algun sospechoso de haberle asesinado (al objeto)
% Evidentemente no hay hechos para esto.


% es(X,Y) sii X es Y (por ejemplo el objeto X es un arma_blanca)

% Para la tercera consulta 
% profesion(X,Y), quizas_tenga(Y,Z), es(Z,arma_blanca). tenemos 2 respuestas
% X = alfonso,Y = carnicero,Z = cuchillo ;X = roberto,Y = carpintero,Z = sierra ;
% Hay hechos para que haya dos respuestas a personas cuya profesion% hace que quiza tenga un objeto
% que es arma blanca

% tiene_pasado_turbio(X) sii X tiene un pasado turbio

% La ultima consulta tiene_pasado_turbio(X), sospechoso(X,_,_). devuelve 2 veces Juan
% Ajsutra con el unico hecho de tiene_pasado_turbio(X)
% No puede ajsutar con las 2 primeras reglas de sospechoso porque El objeto% de un informatico (juan)
% no esta en asesinado_con. Pero en la tercera regla de sospechoso
% hay una colucion por cada hecho de asesinado_con y hay 2.


% Un cambio interesante para la ultima consulta es añadir un asesinado con 
% un objeto de los que quizas tienen los informáticos.  Juan será respuesta muchas más veces.
% Añadiendo asesinado_con(alfonso, ordenador). Juan es solución 4 veces


% Ejercicio 2

% Dos hechos, no se cual se me va a acabar antes
mezcla([],_,[]).
mezcla(_,[],[]).

mezcla([X|Xs],[Y|Ys],[X,Y|Z]) :- mezcla(Xs, Ys, Z).

% Ejercicio 3

% Apartado a)
prefijo([], _).
prefijo([X|Xs], [X|Ys]) :- prefijo(Xs, Ys).

sublistaCons(X,Y):- prefijo(X,Y).
sublistaCons(X,[_|Ys]):- sublistaCons(X,Ys).

% Apartado b)

sublista([], _).
sublista([X|Xs], Y):- member(X,Y), sublista(Xs, Y).

% Ejercicio 5

suma(c,_,c).
suma(s(X),Y, s(Z)) :- suma(X,Y,Z).

numNodos(void, c).
numNodos(arbol_bin(_, I, D), s(Z)):- numNodos(I, W), numNodos(D, Y), suma(W,Y,Z).