:- dynamic male/1.
:- dynamic female/1.
:- dynamic father/1.
:- dynamic mother/1.
:- dynamic child/1.
:- dynamic parent/2.
:- dynamic siblings/2.
:- dynamic adult/1.

father(X) :- male(X).
mother(X) :- female(X).

parent(X,Y) :- father(X), child(Y);
               mother(X), child(Y).

siblings(X,Y) :- parent(Z,X), parent(Z,Y).
