/************* ORDENACION DE LISTAS ***************

Presentamos distintas implementaciones de un predicado

ordena(Xs,Ys) <-> Ys es el resultado de ordenar la lista Xs

Suponemos que las listas tienen términos Prolog cualesquiera
para los que se utilizada el orden estandar @<
***************************************************/

/********** Ordenacion por permutacion  ***********
El  metodo de ordenacion mas ineficiente jamas inventado,
pero que se ajusta al esquema general 'genera y comprueba'
de resolucion de problemas, que consiste en ir generando
soluciones candidatas a un problema, comprobando despues si
realmente lo son o no.
*************************************************/

ordena_permutacion(Xs,Ys) :-
  permutacion(Xs,Ys),    % generacion de candidato
  ordenada(Ys),          % comprobacion de su validez
  !.                     % ¿motivo de este corte?

% permutacion(Xs,Ys) <-> Ys es una permutacion de Xs

permutacion([],[]) :- !.
permutacion(Xs,[X|Ys]) :-
  selecciona(Xs,X,Resto),
  permutacion(Resto,Ys).

% selecciona(Xs,X,Resto) <-> X es un elemento de Xs, y Resto
%                            es la lista de los demas elementos de Xs

selecciona([X|Xs],X,Xs).
selecciona([X|Xs],Y,[X|Ys]) :-
  selecciona(Xs,Y,Ys).

%ordenada(Xs) <-> Xs es una lista ordenada
ordenada([]).
ordenada([X]).
ordenada([X,Y|Xs]) :-
  X @=< Y,
  ordenada([Y|Xs]).

/********** Ordenacion por insercion  ***********/

ordena_insercion([],[]).
ordena_insercion([X|Xs],Ys) :- 
  ordena_insercion(Xs,Zs),
  inserta(X,Zs,Ys).

% inserta(X,Ys,Zs) <-> Zs es el resultado de insertar ordenadamente X
%                      en la lista, que se supone ordenada, Ys 
inserta(X,[],[X]).
inserta(X,[Y|Ys],[X,Y|Ys]) :- 
  X @=< Y.
inserta(X,[Y|Ys],[Y|Zs]) :- 
  X @> Y,
  inserta(X,Ys,Zs).
  
/********** Ordenacion por burbuja  ***********/

ordena_burbuja(Xs,Ys) :-
   hay_burbuja(Xs,Zs),
   !,
   ordena_burbuja(Zs,Ys).
    
ordena_burbuja(Xs,Xs).

% hay_burbuja(Xs,Ys) <-> en la lista Xs hay dos elementos X,Y consecutivos
%    tales que X @> Y, e Ys es el resultado de intercambiarlos

hay_burbuja([X,Y|Xs],[Y,X|Xs]) :-
  X @> Y.
hay_burbuja([X|Xs],[X|Ys]) :-
  hay_burbuja(Xs,Ys).
    
/********** Quicksort de Hoare  ***********/

quicksort([],[]).
quicksort([X|Xs],Ys) :-
  separa(X,Xs,Menores,Mayores),
  quicksort(Menores,Us),
  quicksort(Mayores,Vs),
  append(Us,[X|Vs],Ys).

separa(X,[Y|Ys],[Y|Us],Vs) :-
  Y @=< X,
  separa(X,Ys,Us,Vs).
    
separa(X,[Y|Ys],Us,[Y|Vs]) :-
  Y @> X,
  separa(X,Ys,Us,Vs).

separa(X,[],[],[]).

% variante con acumulador, para evitar append

quicksort1(Xs,Ys) :- 
  quicksort1(Xs,[],Ys).

% quicksort1(Xs,Acum,Ys) <-> Ys resulta de concatenar el resultado de ordenar Xs con Acum
quicksort1([],Acum,Acum).
quicksort1([X|Xs],Acum,Ys) :-
  separa(X,Xs,Menores,Mayores),
  quicksort1(Mayores,Acum,Acum1),      
  quicksort1(Menores,[X|Acum1],Ys).   

/********** Mergesort: ordenacion por mezcla  ***********/

mergesort([],[]) :- !.
mergesort([X],[X]) :- !.
mergesort(Xs,Ys) :-
  mitades(Xs,Us,Vs),
  mergesort(Us,Us1),
  mergesort(Vs,Vs1),
  merge(Us1,Vs1,Ys).

% mitades(Xs,Us,Vs) <-> Us, Vs son el resultado de separar una lista Xs de longitud L en dos listas 
%                       de longitud L/2 si L es par, o (L+1)/2 y (L-1)/2 si L es impar
% merge(Xs,Ys,Zs) <-> Zs es la lista ordenada que resulta de mezclar las listas ordenadas Xs,Ys

merge([],Ys,Ys).
merge(Xs,[],Xs).
merge([X|Xs],[Y|Ys],[X|Zs]) :-
  X @=< Y,
  merge(Xs,[Y|Ys],Zs). 
merge([X|Xs],[Y|Ys],[Y|Zs]) :-
  X @> Y,
  merge([X|Xs],Ys,Zs). 

mitades([],[],[]).
mitades([X],[X],[]).
mitades([X,Y|Xs],[X|Us],[Y|Vs]) :-
  mitades(Xs,Us,Vs).

% Variante de  mitades/3 que separa Xs en dos mitades respetando el orden original
% Podriamos haciendo con dos recorridos de Xs, uno para calcular la longitud de Xs y otro
% para separar el prefijo y el sufijo acordes con esa longitud.
% En lugar de eso, hacemos un solo recorrido, pero en dos copias de Xs.
% En la segunda nos vamos comiendo elementos de dos en dos, de modo que cuando
% esta segunda copia se agote, en la primera hemos llegado hasta la mitad.

mitades1(Xs,Us,Vs) :- 
  mitades1(Xs,Xs,Us,Vs).
  
mitades1(Xs,[],[],Xs).
mitades1([X|Xs],[_],[X],Xs).
mitades1([X|Xs],[_,_|Ys],[X|Us],Vs) :-
  mitades1(Xs,Ys,Us,Vs).

/*********** Generacion de listas aleatorias para pruebas ************/
% Utiliza random/1 de SWI
% rlist(+N,-L) <-> L es una lista de lontigud N con elementos aleatorios entre 0 y N
% rlist(+N,-K,-L) <-> L es una lista de longitud aleatoria K entre 0 y N, con elementos aleatorios
%                   entre 0 y K

rlist(N,L) :-
  rlistaux(0,N,L).

rlistaux(N,N,[]) :-
  !.
  
rlistaux(K,N,[M|L]) :-
  !,
  M is random(N),
  K1 is K+1,
  rlistaux(K1,N,L).
  
rlist(N,K,L) :-
  K is random(N),
  rlist(K,L).

