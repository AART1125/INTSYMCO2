:- dynamic male/1.
:- dynamic parent/1.
:- dynamic female/1.
:- dynamic child/1.
:- dynamic father/2.
:- dynamic mother/2.

parent(X) :- male(X);
             female(X).
son(X,Y) :- child(X), male(X), parent(Y).
daugther(X,Y) :- child(X), female(X), parent(Y).
father(X,Y) :- parent(X), male(X), son(Y,X), X\=Y;
               parent(X), male(X), daugther(Y,X), X\=Y;
               parent(X), male(X), child(Y), X\=Y.
mother(X,Y) :- female(X), son(Y,X);
               female(X), daugther(Y, X);
               parent(X), female(X), child(Y).
