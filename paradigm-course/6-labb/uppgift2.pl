konsonant(X) :- member(X,[66,67,68,70,71,72,74,75,76,77,78,80
                         ,81,82,83,84,86,87,88,90,98,99,100
                         ,102,103,104,106,107,108,109,110,112
                         ,113,114,115,116,118,119,120,122]).



rovarsprak([],[]).
rovarsprak([X|XS],[X,Y,X|YS]) :- Y is 111, konsonant(X), rovarsprak(XS,YS).
rovarsprak([X|XS],[Y|YS]) :- X = Y, not(konsonant(X)), rovarsprak(XS,YS).
