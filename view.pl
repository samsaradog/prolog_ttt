%% view.pl

:- module(view,[display_grid/2]).

display_grid([],[]).
display_grid([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  write(' '), view_cell(G1,P1),
  write(' | '),view_cell(G2,P2),
  write(' | '),view_cell(G3,P3),nl,
  write('-----------'),nl,
  display_grid(GT,PT), !.

view_cell(X,Y) :- var(X), write(Y).
view_cell(X,_Y) :- \+var(X),write(X).
