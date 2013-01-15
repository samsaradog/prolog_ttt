%% tictactoe.pl

initializeGrid :-
  retractall(grid(_)),
  assert(grid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9])).

addMoveToGrid(Player, [X|_],1) :- var(X), X=Player.
addMoveToGrid(Player, [_,X|_],2) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,X|_],3) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,X|_],4) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,_,X|_],5) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,_,_,X|_],6) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,_,_,_,X|_],7) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,_,_,_,_,X|_],8) :- var(X), X=Player.
addMoveToGrid(Player, [_,_,_,_,_,_,_,_,X|_],9) :- var(X), X=Player.

playerMove(Player,Position) :-
  grid(Current),
  addMoveToGrid(Player,Current,Position),
  retract(grid(_)),
  assert(grid(Current)), !. 

checkWinner(Player) :- winner(Player), write(Player), write(' has won!').

%% Rows
findWinner(Player, [G1,G2,G3, _, _, _, _, _, _]) :- G1 == Player,G2 == Player,G3 == Player.
findWinner(Player, [ _, _, _,G4,G5,G6, _, _, _]) :- G4 == Player,G5 == Player,G6 == Player.
findWinner(Player, [ _, _, _, _, _, _,G7,G8,G9]) :- G7 == Player,G8 == Player,G9 == Player.

%% Columns
findWinner(Player, [G1, _, _,G4, _, _,G7, _, _]) :- G1 == Player,G4 == Player,G7 == Player.
findWinner(Player, [ _,G2, _, _,G5, _, _,G8, _]) :- G2 == Player,G5 == Player,G8 == Player.
findWinner(Player, [ _, _,G3, _, _,G6, _, _,G9]) :- G3 == Player,G6 == Player,G9 == Player.

%% Diagonals
findWinner(Player, [G1, _, _, _,G5, _, _, _,G9]) :- G1 == Player,G5 == Player,G9 == Player.
findWinner(Player, [ _, _,G3, _,G5, _,G7, _, _]) :- G3 == Player,G5 == Player,G7 == Player.

winner(Player) :-
  grid(Current),
  findWinner(Player,Current), !.

gridPositions([1,2,3,4,5,6,7,8,9]).

viewCell(X,Y) :- var(X), write(Y).
viewCell(X,_Y) :- \+var(X),write(X).

displayGrid([],[]).
displayGrid([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  write(' '), viewCell(G1,P1),write(' | '),viewCell(G2,P2),write(' | '),viewCell(G3,P3),nl,
  write('-----------'),nl,
  displayGrid(GT,PT), !.

move(Player,Position) :-
  playerMove(Player,Position),
  showGame, 
  not(checkWinner(Player)), !.

newGame :-
  initializeGrid,
  gridPositions(Positions),
  grid(GameState),
  displayGrid(GameState,Positions).

showGame :-
  gridPositions(Positions),
  grid(GameState),
  displayGrid(GameState,Positions).

