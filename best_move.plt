%% best_move.plt

:- use_module(best_move).
:- use_module(grid).
:- use_module(shuffle).

:- begin_tests(grid_values).

test(x_winning_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	fetch_grid(GridState),find_moves(GridState,Moves),
	grid_value(x,Moves,GridState,-1),!.

test(o_winning_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	fetch_grid(GridState),find_moves(GridState,Moves),
	grid_value(o,Moves,GridState,1),!.

test(draw_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	fetch_grid(GridState),find_moves(GridState,Moves),
	grid_value(x,Moves,GridState,0),!.

:- end_tests(grid_values).

:- begin_tests(best_move).

%% Fail if the grid is full

test(best_move0, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	not(best_move(_)).

%% Last empty space on the grid

test(best_move1, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),
	best_move("9").

test(best_move2, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	best_move("1").

test(best_move3, [ setup(initialize_grid), fail ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	best_move("9").
	
%% Make a winning move

test(best_move4, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),
	best_move("9").

test(best_move5, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),                
	                
	best_move("1").

test(best_move6, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),player_move(o,"6"),                

	best_move(X) ->(X=="1";X=="9"),!.
	
test(best_move7, [ setup(initialize_grid) ] ) :-
	                player_move(x,"2"),player_move(o,"3"),
	                player_move(x,"5"),player_move(o,"6"),                

	best_move("9").

%% Block a winning move

test(best_move8, [ setup(initialize_grid) ] ) :-
	                   player_move(x,"2"),player_move(x,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(o,"7"),
	best_move("1").

test(best_move9, [ setup(initialize_grid) ] ) :-
	                                   player_move(o,"3"),
	                player_move(x,"5"),player_move(x,"6"),
	                                
	best_move("4").

%% Pick any position

test(best_move10, [ setup(initialize_grid) ] ) :-
	                   player_move(x,"2"),player_move(o,"3"),
	                   player_move(o,"5"),player_move(x,"6"),
	player_move(x,"7"),                                
	best_move(X) -> (X=="1";X=="4";X=="8";X=="9"),!.
	
%% Don't pick a corner 
	
test(best_move11, [ setup(initialize_grid) ] ) :-
	                                      player_move(x,"3"),
	                   player_move(o,"5"),                
	player_move(x,"7"),
	best_move(X), "9" =\= X, "1" =\= X.

%% If the middle is available, take it.

test(best_move12, [ setup(initialize_grid) ] ) :-
	best_move("5").


%% If the middle is taken, take a corner.

test(best_move13, [ setup(initialize_grid) ] ) :-
	player_move(x,"5"),
	best_move(X) -> (X=="1";X=="3";X=="7";X=="9"),!.


:- end_tests(best_move).


