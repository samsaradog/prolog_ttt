:- include(tictactoe).
:- use_module(library(apply)).

%%
%% Test setting up the board
%%

allVar(List) :- maplist(var, List).

someVar([_A,o,_B]).

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
  playerMove(o,2),grid(Tmp),moveTwo(Tmp).

test(tictactoe_move3, [ fail ] ) :-
  playerMove(x,1),move(x,1).

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

test(tictactoe_win10, [ setup(initializeGrid), fail ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	winner(o).

:- end_tests(tictactoe_win).

%%
%% Test finding the best move
%%

:- begin_tests(tictactoe_get_move).

%%
%% Last empty space on the grid
%%

test(tictactoe_get_move1, [ setup(initializeGrid) ] ) :-
	playerMove(o,1),playerMove(o,2),playerMove(x,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),
	bestMove(9).

test(tictactoe_get_move2, [ setup(initializeGrid) ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	bestMove(1).

test(tictactoe_get_move3, [ setup(initializeGrid), fail ] ) :-
	                playerMove(o,2),playerMove(o,3),
	playerMove(o,4),playerMove(x,5),playerMove(x,6),
	playerMove(x,7),playerMove(x,8),playerMove(o,9),
	bestMove(9).
	
%%
%% Block a winning move
%%

test(tictactoe_get_move4, [ setup(initializeGrid) ] ) :-
	                                                
	                playerMove(x,5),                
	                playerMove(x,8),playerMove(o,9),
	bestMove(2).

:- end_tests(tictactoe_get_move).
