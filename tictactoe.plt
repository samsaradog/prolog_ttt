:- include(tictactoe).
:- use_module(library(apply)).

allVar(List) :- maplist(var, List).

someVar([_A,o,_B]).

:- begin_tests(extractMove).

test(extract_move1) :-
	extractMove([0,-1,-1],[1,2,3],1).

test(extract_move2) :-
	extractMove([0,1,-1],[1,2,3],2).

test(extract_move3) :-
	extractMove([0,-1,1],[1,2,3],3).

:- end_tests(extractMove).

:- begin_tests(grid_values).

test(x_winning_grid_value, [setup(initializeGrid)] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(o,6),
	playerMove(x,7),playerMove(o,8),playerMove(o,9),
	grid(GridState),findMoves(GridState,Moves),
	gridValue(x,Moves,GridState,-1).

test(o_winning_grid_value, [setup(initializeGrid)] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(o,6),
	playerMove(x,7),playerMove(o,8),playerMove(o,9),
	grid(GridState),findMoves(GridState,Moves),
	gridValue(o,Moves,GridState,1).

test(draw_grid_value, [setup(initializeGrid)] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(o,8),playerMove(o,9),
	grid(GridState),findMoves(GridState,Moves),
	gridValue(x,Moves,GridState,0).

:- end_tests(grid_values).

%%
%% Test setting up the board
%%

:- begin_tests(tictactoe_setup, [setup(initializeGrid)] ).

test(tictactoe3, [ setup(someVar(Tmp)), fail ] ) :-
  allVar(Tmp). 
        
test(tictactoe4) :-
  grid(Tmp),allVar(Tmp). 
        
:- end_tests(tictactoe_setup).

%%
%% Test making moves
%%

moveTwo([_,o|_]).

:- begin_tests(tictactoe_move, [ setup(initializeGrid) ] ).

test(tictactoe_move1, [ fail ] ) :-
  playerMove(x,1),grid(Tmp),allVar(Tmp).

test(tictactoe_move2) :-
  not(playerMove(x,1)).

test(tictactoe_move3) :-
  playerMove(o,2),grid(Tmp),moveTwo(Tmp).

:- end_tests(tictactoe_move).

%%
%% Test winning combinations
%%

:- begin_tests(tictactoe_win).

test(tictactoe_win1, [ setup(initializeGrid) ] ) :-
  playerMove(x,1),playerMove(x,2),playerMove(x,3),winner(x).

test(tictactoe_win2, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(o,6),
	playerMove(x,7),playerMove(o,8),playerMove(o,9),
	winner(x).

test(tictactoe_win3, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(x,2),playerMove(o,3),
	playerMove(x,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(o,8),playerMove(o,9),
	winner(x).

test(tictactoe_win4, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(x,2),playerMove(o,3),
	playerMove(x,4),playerMove(o,5),playerMove(o,6),
	playerMove(x,7),playerMove(x,8),playerMove(x,9),
	winner(x).

test(tictactoe_win5, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(o,3),
	playerMove(x,4),playerMove(o,5),playerMove(o,6),
	playerMove(x,7),playerMove(o,8),playerMove(x,9),
	winner(x).

test(tictactoe_win6, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(x,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	winner(x).

test(tictactoe_win7, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(o,5),playerMove(x,6),
	playerMove(o,7),playerMove(x,8),playerMove(x,9),
	winner(x).

test(tictactoe_win8, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(o,7),playerMove(x,8),playerMove(x,9),
	winner(x).

test(tictactoe_win9, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	winner(x).

test(tictactoe_win10, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	not(winner(o)).

:- end_tests(tictactoe_win).

%%
%% Test finding the best move
%%

:- begin_tests(tictactoe_best_move).

%% Fail if the grid is full

test(tictactoe_best_move0, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	not(bestMove(_)).

%% Last empty space on the grid

test(tictactoe_best_move1, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),
	bestMove(9).

test(tictactoe_best_move2, [ setup(initializeGrid) ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	bestMove(1).

test(tictactoe_best_move3, [ setup(initializeGrid), fail ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	bestMove(9).
	
%% Make a winning move

test(tictactoe_best_move4, [ setup(initializeGrid) ] ) :-
	playerMove(x,1),playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(o,6),
	playerMove(x,7),
	bestMove(9).

test(tictactoe_best_move5, [ setup(initializeGrid) ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(x,4),playerMove(x,5),                
	                
	bestMove(1).

test(tictactoe_best_move6, [ setup(initializeGrid) ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(x,4),playerMove(x,5),playerMove(o,6),                

	bestMove(1).
	
test(tictactoe_best_move7, [ setup(initializeGrid) ] ) :-
	                playerMove(x,2),playerMove(o,3),
	                playerMove(x,5),playerMove(o,6),                

	bestMove(9).

%% Block a winning move

test(tictactoe_best_move8, [ setup(initializeGrid) ] ) :-
	                playerMove(x,2),playerMove(x,3),
	playerMove(x,4),playerMove(o,5),playerMove(o,6),
	playerMove(o,7),
	bestMove(1).

test(tictactoe_best_move8, [ setup(initializeGrid) ] ) :-
	                                playerMove(o,3),
	                playerMove(x,5),playerMove(x,6),
	                                
	bestMove(4).

%% Pick any position

test(tictactoe_best_move4, [ setup(initializeGrid) ] ) :-
	                playerMove(x,2),playerMove(o,3),
	                playerMove(o,5),playerMove(x,6),
	playerMove(x,7),                                
	bestMove(X) -> (X==1;X==4;X==8;X==9),!.
	
%% Don't pick a corner 
	
test(tictactoe_best_move10, [ setup(initializeGrid) ] ) :-
	                                playerMove(x,3),
	                playerMove(o,5),                
	playerMove(x,7),
	bestMove(X), 9 =\= X, 1 =\= X.

%% If the middle is available, take it.

test(tictactoe_best_move10, [ setup(initializeGrid) ] ) :-
	bestMove(5).


:- end_tests(tictactoe_best_move).
