:- dynamic male/1.
:- dynamic female/1.
:- dynamic father/2.
:- dynamic mother/2.
:- dynamic child/2.
:- dynamic son/2.
:- dynamic daughter/2.
:- dynamic parent/2.
:- dynamic uncle/2.
:- dynamic aunt/2.
:- dynamic siblings/2.
:- dynamic sister/2.
:- dynamic brother/2.
:- dynamic grandparent/2.
:- dynamic grandmother/2.
:- dynamic grandfather/2.
:- discontiguous parent/2.
:- discontiguous siblings/2.

child(X,Y) :- parent(Y,X).
child(X,Y) :- son(X,Y).
child(X,Y) :- daughter(X,Y).

father(X,Y) :- male(X), parent(Y,X), X\=Y,!.
mother(X,Y) :- female(X), parent(Y,X), X\=Y,!.

son(X,Y) :- male(X), (father(Y,X);mother(Y,X);parent(Y,X)).
daughter(X,Y) :- female(X), (father(Y,X);mother(Y,X);parent(Y,X)).

siblings(X,Y) :- (parent(Z,X), parent(Z,Y));(mother(Z,X),mother(Z,Y));(father(Z,X),father(Z,Y)), X\=Y. 

sister(X, Y) :- female(X), siblings(X,Y), X \= Y.
brother(X,Y) :- male(X), siblings(X,Y), X\=Y.

grandfather(X,Y) :- male(X), father(X,Z), parent(Z,Y).
grandfather(X,Y) :- male(X), father(X,Z), mother(Z,Y).
grandfather(X,Y) :- male(X), father(X,Z), father(Z,Y).

grandmother(X,Y) :- female(X), mother(X,Z), parent(Z,Y).
grandmother(X,Y) :- female(X), mother(X,Z), mother(Z,Y).
grandmother(X,Y) :- female(X), mother(X,Z), father(Z,Y).

uncle(X,Y) :- male(X), brother(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), sister(X,Z), parent(Z,Y).

relatives(X, Y) :- siblings(X, Y).
relatives(X, Y) :- parent(X, Z), parent(Y, Z), X \= Y.
relatives(X, Y) :- parent(X, Z), siblings(Y, Z), X \= Y.
relatives(X, Y) :- parent(Y, Z), siblings(X, Z), X \= Y.
relatives(X, Y) :- aunt(X, Y).
relatives(X, Y) :- uncle(X, Y).
relatives(X, Y) :- grandfather(X, Y).
relatives(X, Y) :- grandmother(X, Y).

