derivada(Y,X,0):- var(Y), Y \==X, !.
derivada(X,X,1):- var(X),!.
derivada(N,X,0):- number(N),!.
derivada(U+V,X,DU + DV):- !, derivada(U,X,DU), derivada(V,X,DV).
derivada(U*V,X,DU*V + DV*U):- !,  derivada(U,X,DU), derivada(V,X,DV).
derivada(U/V,X,(DU*V - DV*U)/(V^2)):- !, derivada(U,X,DU), derivada(V,X,DV).