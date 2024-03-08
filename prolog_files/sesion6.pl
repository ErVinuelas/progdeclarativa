% Autor:  Pablo Martín Viñuelas
% Fecha: 28/11/2022
% Hechos:

cima(d).
sobre(d,c).
sobre(c,b).
sobre(b,a).


cima(g).
sobre(g,f).
sobre(f,e).


cima(i).
sobre(i,h).


izquierda(c,g).

izquierda(b,f).
izquierda(f,i).

izquierda(a,e).
izquierda(e,h).

% Se definen nuevos predicados para manejar esta información.


% por_encima_de(X,Y) <-> la ficha X esta en la misma pila que la ficha Y y más arriba.
% uso: por_encima_de(e/s,e/s).
por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

% por_arriba_ls(X,L) <-> L es la lista que contiene todas las fichas que están por encima de la ficha X.
% uso:  por_arriba_ls(e/s,e/s).
por_arriba_ls(X,[]) :- cima(X).
por_arriba_ls(X,[Y|L]) :- sobre(Y,X), por_arriba_ls(Y,L).

% Ejercicio1

% lista_mas_larga(Lx, Ly) si y solo si Lx tiene mas elementos

lista_mas_larga([X|Lx], []).
lista_mas_larga([X|Lx], [Y| Ly]) :- lista_mas_larga(Lx, Ly).

% mas por encima que(X,Y) ? la ficha X tiene m ´as fichas por encima suya, dentro de su pila, que fichas
% tiene la ficha Y por encima, dentro de su propia pila.

mas_por_encima_que(X, Y) :- por_arriba_ls(X, Lx), por_arriba_ls(Y, Ly), lista_mas_larga(Lx, Ly).

%Ejercicio 2

%intercala(L1,L2,L) ? L1 y L2 son dos listas y L es la lista resultante de intercalar los elementos de L1
% y L2 y cuya longitud es igual al doble de la lista de menor longitud.

intercala(L1, [], []).
intercala([], L2, []).
intercala([X|L1], [Y|L2], [X| [Y| L]]) :- intercala(L1, L2, L).

%Ejercicio 3

%contenida(Xs,Ys) ? Xs e Y s son listas dadas cuyos elementos pueden ordenarse, aparecen ordenados
%de menor a mayor, y son tales que cada elemento de Xs es un elemento de Ys, independientemente del
%n ´umero de veces que aparezca en Xs.

contenida([], L).
contenida([X| Xs], [Y| Ys]) :- X = Y, contenida(Xs, Ys).
contenida([X| Xs], [Y| Ys]) :- X = Y, contenida(Xs, [Y| Ys]).
contenida([X| Xs], [Y| Ys]) :- X @> Y, contenida([X| Xs], Ys).


%Ejercicio 4
%listas iguales
listas_iguales([],[]).
listas_iguales([X1| L1], [X2| L2]) :- X1 = X2, listas_iguales(L1, L2).

%sufijos(Xs,Yss) ? Yss es una lista cuyos elementos son las listas que son sufijos de la lista Xs.
sufijos([], [[]]).
sufijos([X| Xs], [Lx| L]) :- listas_iguales([X| Xs], Lx), sufijos(Xs, L).


%Ejercicio 5

%Definicion de arbol
arbol_binario(void).
arbol_binario(arbol(_, I, D)):- arbol_binario(I), arbol_binario(D).


%numnodos(X,Y) ? X es el n ´umero de nodos del  ´arbol binario Y

numnodos(0, void).
numnodos(Z, arbol_binario(_, I, D)) :- numnodos(X, I), numnodos(Y, D), Z is X+Y+1.

