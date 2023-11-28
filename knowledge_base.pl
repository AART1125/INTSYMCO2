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
:- dynamic grandmother/2.
:- dynamic grandfather/2.
:- dynamic grandchild/2.
:- dynamic grandson/2.
:- dynamic granddaughter/2.
:- discontiguous father/2.
:- discontiguous mother/2.

father(X,Y) :- male(X), parent(X,Y), X\=Y,!.
mother(X,Y) :- female(X), parent(X,Y), X\=Y,!.

father(X,Y) :- male(X), parent(X,Y), X\=Y,!.
mother(X,Y) :- female(X), parent(X,Y), X\=Y,!.

son(X,Y) :- male(X), (father(Y,X);mother(Y,X);parent(Y,X)), X \= Y.
daughter(X,Y) :- female(X), (father(Y,X);mother(Y,X);parent(Y,X)), X \= Y.

siblings(X,Y) :- (parent(Z,X), parent(Z,Y));(mother(Z,X),mother(Z,Y));(father(Z,X),father(Z,Y)), X\=Y. 

sister(X,Y) :- female(X), siblings(X,Y), X\=Y.
brother(X,Y) :- male(X), siblings(X,Y), X\=Y.

grandfather(X,Y) :- male(X), father(X,Z), parent(Z,Y), X \= Y.
grandfather(X,Y) :- male(X), father(X,Z), mother(Z,Y), X \= Y.
grandfather(X,Y) :- male(X), father(X,Z), father(Z,Y), X \= Y.

grandmother(X,Y) :- female(X), mother(X,Z), parent(Z,Y), X \= Y.
grandmother(X,Y) :- female(X), mother(X,Z), mother(Z,Y), X \= Y.
grandmother(X,Y) :- female(X), mother(X,Z), father(Z,Y), X \= Y.

grandchild(X, Y) :- child(X, Z), (grandfather(Y,Z); grandmother(Y,Z)), X \= Y.
grandson(X, Y) :- son(X, Z), (grandfather(Y,Z); grandmother(Y,Z)), X \= Y.
granddaughter(X, Y) :- daughter(X, Z), (grandfather(Y,Z); grandmother(Y,Z)), X \= Y.

uncle(X,Y) :- male(X), brother(X,Z), parent(Z,Y), X \= Y.
aunt(X,Y) :- female(X), sister(X,Z), parent(Z,Y), X \= Y.

thirddegree(X,Y) :- siblings(X,Z), parent(Z,Y), X \= Y.
thirddegree(X,Y) :- parent(W,X), parent(Z,Y), (siblings(W,Z);siblings(Z,W)), X \= Y.

relatives(X,Y) :- siblings(X,Y), X \= Y.
relatives(X,Y) :- parent(X,Z), parent(Y,Z), X \= Y.
relatives(X,Y) :- parent(X,Z), siblings(Y,Z), X \= Y.
relatives(X,Y) :- parent(Y,Z), siblings(X,Z), X \= Y.
relatives(X,Y) :- aunt(X,Y), X \= Y.
relatives(X,Y) :- uncle(X,Y), X \= Y.
relatives(X,Y) :- thirddegree(X,Y), X \= Y.
relatives(X,Y) :- grandfather(X,Y), X \= Y.
relatives(X,Y) :- grandmother(X,Y), X \= Y.
relatives(X,Y) :- grandchild(X,Y), X \= Y.
relatives(X,Y) :- grandson(X,Y), X \= Y.
relatives(X,Y) :- granddaughter(X,Y), X \= Y.