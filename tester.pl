:- discontiguous male/1.
:- discontiguous female/1.
:- discontiguous parent/2.
    
sibling(X,Y):- 
    X \= Y,
    parent(Z,X), 
    parent(Z,Y).

child(X,Y) :- 
    X \= Y,
    parent(Y,X).



grandparent(X,Y) :- 
    X \= Y,
    parent(X,Z), 
    parent(Z,Y).

mother(X,Y):- 
    X \= Y,
    female(x),
    parent(X,Y).

father(X,Y):- 
    X \= Y,
    male(X),
	parent(X,Y). 

son(X,Y) :- 
    X \= Y,
    male(X),
    child(X,Y).

daughter(X,Y) :- 
    X \= Y,
    female(X),  
    child(X,Y). 

brother(X,Y) :- 
    X \= Y,
    male(X),
    sibling(X,Y).

sister(X,Y) :- 
    X \= Y,
    female(X),  
    sibling(X,Y). 

grandmother(X,Y):- 
    X \= Y,
    female(X),
    grandparent(X,Y).

grandfather(X,Y):- 
    X \= Y,
    male(X),
	grandparent(X,Y). 
    
aunt(X,Y) :- 
    X \= Y,
    female(X),
	sibling(X,Z), 
	parent(Z,Y).

uncle(X,Y) :- 
    X \= Y,
    male(X),
	sibling(X,Z), 
	parent(Z,Y).

children(W,X,Y,Z) :-
    W \= X,
    W \= Y,
    W \= Z,
    X \= Y,
    X \= Z,
    Y \= Z,
    child(W,Z),
    child(X,Z),
    child(Y,Z).

parents(X,Y,Z) :-
    X \= Y,
    X \= Z,
    Y \= Z,
    parent(X,Z),
    parent(Y,Z).

relative(X,Y) :- 
    X \= Y,
    (parent(X,Y); parent(Y,X)); 
    (grandparent(X,Y); grandparent(Y,X)); 
    (uncle(X,Y); uncle(Y,X));
    (aunt(X,Y); aunt(Y,X));
    sibling(X,Y).
    
   
% Example 1: true
male(bob).
female(alice).
male(john).
parent(bob,alice).
parent(alice,john).
% grandfather(bob,john).

% Example 2: false
male(mark).
female(patricia).
female(ann).
parent(mark,patricia).
% daughter(mark,ann).

% Example 3: false
male(one).
female(two).
parent(one,two).
parent(two,three).
% father(three,one).

% Example 4: false
female(lea).
% mother(lea, lea).

% Testing:
male(ace).
male(robee).
parent(ace,robee).
parent(ace,derrick).
parent(ace,lee).
% Need to list siblings

