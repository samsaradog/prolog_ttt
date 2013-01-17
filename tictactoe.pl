%% tictactoe.pl

%% Any similarity to lisp is intentional

car([H|_],Element) :- Element = H.

%% Grid goals

initializeGrid :-
  retractall(grid(_)),
  assert(grid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9])).

matchMoveToGrid(Player, [X|_],1) :- var(X), X=Player.
matchMoveToGrid(Player, [_,X|_],2) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,X|_],3) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,X|_],4) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,_,X|_],5) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,_,_,X|_],6) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,_,_,_,X|_],7) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,_,_,_,_,X|_],8) :- var(X), X=Player.
matchMoveToGrid(Player, [_,_,_,_,_,_,_,_,X|_],9) :- var(X), X=Player.

dupGridWithMove(P,[G1|T],[P|T],1) :- var(G1).
dupGridWithMove(P,[G1,G2|T],[G1,P|T],2) :- var(G2).
dupGridWithMove(P,[G1,G2,G3|T],[G1,G2,P|T],3) :- var(G3).
dupGridWithMove(P,[G1,G2,G3,G4|T],[G1,G2,G3,P|T],4) :- var(G4).
dupGridWithMove(P,[G1,G2,G3,G4,G5|T],[G1,G2,G3,G4,P|T],5) :- var(G5).
dupGridWithMove(P,[G1,G2,G3,G4,G5,G6|T],[G1,G2,G3,G4,G5,P|T],6) :- var(G6).
dupGridWithMove(P,[G1,G2,G3,G4,G5,G6,G7|T],[G1,G2,G3,G4,G5,G6,P|T],7) :- var(G7).
dupGridWithMove(P,[G1,G2,G3,G4,G5,G6,G7,G8|T],[G1,G2,G3,G4,G5,G6,G7,P|T],8) :- var(G8).
dupGridWithMove(P,[G1,G2,G3,G4,G5,G6,G7,G8,G9|T],[G1,G2,G3,G4,G5,G6,G7,G8,P|T],9) :- var(G9).

playerMove(Player,Position) :-
  grid(Current),
  matchMoveToGrid(Player,Current,Position),
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

swapPlayer(o,x).
swapPlayer(x,o).

findMoves(GridState,Moves) :- findall(X,matchMoveToGrid(o,GridState,X),Moves).

bestMove(Move) :-
	grid(GridState),
	findMoves(GridState,Moves),
	findBestMove(o,Move,Moves,GridState), !.

%% when the list of available moves is empty, fail

findBestMove(_Player,_Move,Moves,_GridState) :- 
	length(Moves,0), !, fail.
	
%% pick the middle if available.

findBestMove(_Player,Move,Moves,_GridState) :-
	member(5,Moves),Move=5.
	
%% pick a corner if only the middle is taken

findBestMove(_Player,Move,Moves,_GridState) :-
	length(Moves,8),
	not(member(5,Moves)),
	Move=1.

%% when best move is the last one on the grid.

findBestMove(_Player,Move,Moves,_GridState) :- 
	length(Moves,1),
	car(Moves,Move).

%% when best move is the winning move

findBestMove(Player,Move,Moves,GridState) :- 
	buildWinners(Moves,Player,GridState,Result), 
	car(Result,Move).
	
%% when best move blocks a winning move
	
findBestMove(Player,Move,Moves,GridState) :-
	swapPlayer(Player,OtherPlayer),
	buildWinners(Moves,OtherPlayer,GridState,Result),
	car(Result,Move).
	
%% When we have to search for the best move. This is only called
%% by the computer at the root. From here down we look at grid values

findBestMove(Player,Move,Moves,GridState) :-
	buildValues(Player,GridState,Moves,Values),
	extractMove(Values,Moves,Move).

extractMove([ValueHead|ValueTail],[MoveHead|MoveTail],Result) :-
	extractMove(ValueTail,ValueHead,MoveTail,MoveHead,Result).
	
extractMove([],_,[],Move,Move).
extractMove([ValueHead|ValueTail],Value,[MoveHead|MoveTail],Move,Result) :-
	ValueHead > Value -> extractMove(ValueTail,ValueHead,MoveTail,MoveHead,Result)
	; extractMove(ValueTail,Value,MoveTail,Move,Result).
	
%% Meta-predicate based on include/3
	
buildWinners([],_,_,[]).
buildWinners([Move|T],Player,GridState,Result) :-
	( dupGridWithMove(Player,GridState,NewGridState,Move),
	  findWinner(Player,NewGridState) -> Result = [Move|NextResult]
	  ; NextResult = Result),
	buildWinners(T,Player,GridState,NextResult).
	
%% Derives value for current grid

gridValue(_,_Moves,GridState,-1) :- findWinner(x,GridState),!.
gridValue(_,_Moves,GridState, 1) :- findWinner(o,GridState),!.
gridValue(_,Moves,_GridState, 0) :- 
	length(Moves,Length),
	0 == Length,!.

gridValue(Player,Moves,GridState,Value) :-
	swapPlayer(Player,OtherPlayer),
	buildValues(OtherPlayer,GridState,Moves,Values),
	extractValue(OtherPlayer,Values,Value).

buildValues(_Player,_GridState,[],[]).
buildValues(Player,GridState,[Move|T],Values) :-
	( dupGridWithMove(Player,GridState,NewGridState,Move),
	  findMoves(NewGridState,Moves),
	  gridValue(Player,Moves,NewGridState,Value) -> Values = [Value|NextValues]
	  ; NextValues = Values),
	buildValues(Player,GridState,T,NextValues).
	
extractValue(x,Values,Value) :- min_list(Values,Value).
extractValue(o,Values,Value) :- max_list(Values,Value).

%%
%% Sugar goals for human players
%%

displayGrid([],[]).
displayGrid([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  write(' '), viewCell(G1,P1),write(' | '),viewCell(G2,P2),write(' | '),viewCell(G3,P3),nl,
  write('-----------'),nl,
  displayGrid(GT,PT), !.

move(Position) :-
  playerMove(x,Position),
  bestMove(X),
  playerMove(o,X),
  showGame, 
  not(checkWinner(o)), !.

newGame :-
  initializeGrid,
  gridPositions(Positions),
  grid(GameState),
  displayGrid(GameState,Positions).

showGame :-
  gridPositions(Positions),
  grid(GameState),
  displayGrid(GameState,Positions).

