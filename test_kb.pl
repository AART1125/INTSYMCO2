:- dynamic male/1.
:- dynamic female/1.
:- dynamic father/1.
:- dynamic mother/1.
:- dynamic child/1. # Maybe change to child/2 
                    # Since yung parent takes 2 arguments so both
                    # are similar
:- dynamic parent/2.

father(X) :- male(X).
mother(X) :- female(X).

parent(X,Y) :- father(X), child(Y);
               mother(X), child(Y).
# Consequently,
# parent(X,Y) :- father(X), child(Y,X);
#                mother(X), child(Y,X).

siblings(X,Y) :- parent(Z,X), parent(Z,Y). 
# siblings(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y.

# OR
# :- dynamic male/1.
# :- dynamic female/1.
# :- dynamic father/2.
# :- dynamic mother/2.

# father(X) :- male(X), parent(X, Y).
# mother(X) :- female(X), parent(X, Y).

# parent(X,Y) :- father(X, Y) ; mother(X, Y).

# siblings(X,Y) :- parent(Z, X), parent(Z, Y), X \= Y.
