%% grid.plt

:- use_module(grid).

%%
%% Test setting up the board
%%

:- begin_tests(grid_setup, [setup(initialize_grid)] ).

someVar([_A,o,_B]).

test(grid_setup1) :-
	someVar(Tmp),
	not(maplist(var,Tmp)). 
        
test(grid_setup2) :-
  fetch_grid(Tmp),maplist(var,Tmp). 
        
:- end_tests(grid_setup).

%%
%% Test making moves
%%

moveTwo([_,o|_]).

:- begin_tests(grid_move, [ setup(initialize_grid) ] ).

test(grid_move1, [ fail ] ) :-
  player_move(x,"1"),fetch_grid(Tmp),maplist(var,Tmp).

test(grid_move2) :-
  not(player_move(x,"1")).

test(grid_move3) :-
  player_move(o,"2"),fetch_grid(Tmp),moveTwo(Tmp).

:- end_tests(grid_move).

%%
%% Test winning combinations
%%

:- begin_tests(grid_win).

test(grid_win1, [ setup(initialize_grid) ] ) :-
  player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),winner(x).

test(grid_win2, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	winner(x).

test(grid_win3, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	winner(x).

test(grid_win4, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(grid_win5, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(x,"9"),
	winner(x).

test(grid_win6, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	winner(x).

test(grid_win7, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(o,"5"),player_move(x,"6"),
	player_move(o,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(grid_win8, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(o,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(grid_win9, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	winner(x).

test(grid_win10, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	not(winner(o)).

:- end_tests(grid_win).

:- begin_tests(grid_draw).

test(grid_draw, [setup(initialize_grid)] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(o,"7"),player_move(x,"8"),player_move(o,"9"),
	draw_game.

:- end_tests(grid_draw).
