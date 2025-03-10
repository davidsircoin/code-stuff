varannan([], []).
varannan([X],[X]).
varannan([X1, _ | XS], [X1|YS]) :- varannan(XS,YS).

jamn([],[]).
jamn([_], []).
jamn([_ , X1 | XS], [X2|YS]) :- jamn(XS,YS).

skyffla([],[]).
skyffla(XS,YS) :- varannan(XS,AS), jamn(XS,BS), skyffla(BS,CS), append(AS,CS,YS), !.


