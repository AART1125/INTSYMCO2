:- dynamic male/1.
:- dynamic female/1.
:- dynamic father/1.
:- dynamic mother/1.
:- dynamic child/2. % Maybe change to child/2 
                    % Since yung parent takes 2 arguments so both
                    % are similar
:- dynamic parent/2.
:- dynamic uncle/2.
:- dynamic aunt/2.
:- dynamic siblings/2.
:- dynamic grandparent/2.
:- discontiguous parent/2.
:- discontiguous siblings/2.

siblings(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

parent(X,Y) :- father(X), child(Y, X), X\=Y, !.
parent(X,Y) :- mother(X), child(Y, X), X\=Y, !.

grandparent(X,Y) :- parent(X,Z), parent(Z,Y), child(Y,Z), X\=Y.

% Consequently,
% parent(X,Y) :- father(X), child(Y,X);
%                mother(X), child(Y,X).

siblings(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y. 
% siblings(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y.
uncle(X,Y) :- male(X), (siblings(X,Z), child(Y,Z)), X\=Y.
aunt(X,Y) :- female(X), (siblings(X,Z), child(Y,Z)), X\=Y.
% OR
% :- dynamic male/1.
% :- dynamic female/1.
% :- dynamic father/2.
% :- dynamic mother/2.

% father(X) :- male(X), parent(X, Y).
% mother(X) :- female(X), parent(X, Y).

% parent(X,Y) :- father(X, Y) ; mother(X, Y).

% siblings(X,Y) :- parent(Z, X), parent(Z, Y), X \= Y.
