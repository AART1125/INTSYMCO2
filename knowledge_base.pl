:- dynamic male/1.
:- dynamic parent/1.
:- dynamic female/1.
:- dynamic child/1.
:- dynamic father/2.
:- dynamic mother/2.

son(X) :- male(X).
daugther(X) :- female(X).

parent(X) :- male(X);
             female(X).
daugther(X,Y) :- child(X), female(X), parent(Y).
father(X,Y) :- parent(X), male(X), son(Y,X), X\=Y;
               parent(X), male(X), daugther(Y,X), X\=Y;
               parent(X), male(X), child(Y), X\=Y.
mother(X,Y) :- parent(X), female(X), son(Y,X), X\=Y;
               parent(X), female(X), daugther(Y, X), X\=Y;
               parent(X), female(X), child(Y), X\=Y.
