%Barrientos Álvarez Jorge Miguel Aarón  |  jma.barrientos@ciencias.unam.mx

% Tribonacci
trib(0, 0):- !.
trib(1, 0):- !. 
trib(2, 1):- !. 
trib(X, Y) :-
    X>2,
    A is X-1,
    B is X-2,
    C is X-3,
    trib(A, D),
    trib(B, E),
    trib(C, F),
    Y is D+E+F,
    !.

% Factorial
fac(0, 1):- !.
fac(X, Y) :-
    X>0,
    A is X-1,
    fac(A, B),
    Y is X*B,
    !.

% Cuenta.
cuenta(0, 0):- !.
cuenta(1, 1):- !.
cuenta(X, Y) :-
    X>1,
    A is X-1,
    cuenta(A, B),
    Y is X+B,
    !.

% Suma
suma([], 0):- !.
suma([X], X):- !.
suma([X, Y | Z], A) :-
    suma([Y | Z], R),
    A is X + R,
    !.

% And y Not (?
not(1, 0).
not(0, 1).

and(1, 1, 1).
and(0, 0, 1).
and(1, 0, 0).
and(0, 1, 0).

% LISTAS

%Contiene
contiene([X|_], X):- !.
contiene([_|X], Z):- contiene(X, Z), !.

% Elimina
borrar(_, [], []):- !.
borrar(X, [X | Y], Y2):- borrar(X, Y, Y2), !.
borrar(X, [Y | Z], [Y|Z2]):- X\=Y, borrar(X, Z, Z2), !.


% ARBOLES
bt(void).
bt(node(A, T1, T2)):- integer(A), bt(T1), bt(T2).


% Elem
elem(A, bt(node(A, _, _))):- !.
elem(A, bt(node(_, T1, _))):- T1\=void, elem(A, T1).
elem(A, bt(node(_,_, T2))):- T2\=void, elem(A, T2).

%MinELem












