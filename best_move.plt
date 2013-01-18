%% best_move.plt

:- use_module(grid).

:- begin_tests(grid_values).

test(x_winning_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	grid(GridState),find_moves(GridState,Moves),
	grid_value(x,Moves,GridState,-1),!.

test(o_winning_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	grid(GridState),find_moves(GridState,Moves),
	grid_value(o,Moves,GridState,1),!.

test(draw_grid_value, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	grid(GridState),find_moves(GridState,Moves),
	grid_value(x,Moves,GridState,0),!.

:- end_tests(grid_values).

