%% tictactoe.pl

initializeGrid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9]).
gridPositions([1,2,3,4,5,6,7,8,9]).

viewCell(X,Y) :- var(X), write(Y).
viewCell(X,_Y) :- \+var(X),write(X).

displayRow([],[]).
displayRow([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  viewCell(G1,P1),write(' '),viewCell(G2,P2),write(' '),viewCell(G3,P3),nl,
  displayRow(GT,PT).

displayGrid :-
  initializeGrid(Grid),
  gridPositions(Positions),
  displayRow(Grid,Positions).
