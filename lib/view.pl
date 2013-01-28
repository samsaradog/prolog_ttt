%% view.pl

:- module(view,[display_grid/2]).

:- use_module(io).

display_grid([],[]).
display_grid([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  user_message(' '),  view_cell(G1,P1),
  user_message(' | '),view_cell(G2,P2),
  user_message(' | '),view_cell(G3,P3),line_break,
  user_message('-----------'),line_break,
  display_grid(GT,PT), !.

view_cell(X,Y)  :-   var(X),user_message(Y).
view_cell(X,_Y) :- \+var(X),user_message(X).
