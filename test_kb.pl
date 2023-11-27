set_prolog_stack(global, limit(16*10**9)).
set_prolog_stack(local, limit(4*10**9)).


:- dynamic male/1.
:- dynamic female/1.
:- dynamic child_of/2.
:- dynamic siblings/2.
:- dynamic adult/1.
:- dynamic mother/2.
:- dynamic father/2.
:- dynamic parent/2.

father(X, Y) :- male(X), child_of(Y, X).
mother(X, Y) :- female(X), child_of(Y, X).
parent(X, Y) :- father(X, Y), !.
parent(X, Y) :- mother(X, Y), !.
siblings(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
child(X) :- child_of(_, X).

child_of(X, Y) :- father(X, Y), assertz(child_of(X, Y)).
child_of(X, Y) :- mother(X, Y), assertz(child_of(X, Y)).