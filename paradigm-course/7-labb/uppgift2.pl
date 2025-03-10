% Syntactically nicer
:- op(100, xfy, ->).
:- op(1000, xfx, ::).
::(X,Y) :- check([],X,Y).

% This is an implementation of a simple type checker in prolog that automatically gives rise to type inference as well for the
% cases covered.

check(_, X, int) :- integer(X). %check/2. Just a primitive 

% Adding check of sums and products
check(G, X + Y, int) :- check(G, X, int), check(G,Y,int). 
check(G, X * Y, int) :- check(G, X, int), check(G,Y,int).


% Adding check of pairs
check(G,(X,Y),(U,V)) :- check(G,X,U), check(G,Y,V).


% Adding check of projections of pairs
check(G,fst(M),A) :- check(G,M,(A,_)).
check(G,snd(M),B) :- check(G,M,(_,B)).  

% Add lambda function type check
check(G, lambda(X,M), A->B) :- 
    check([X :: A | G], M, B).

check(G, app(M,N), B) :- check(G, N, A), check(G, M, A -> B).

check([X :: A | _], X, B) :- unify_with_occurs_check(A,B).
check([_| G], X, B) :- check(G, X, B).


