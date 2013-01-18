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
  grid(Tmp),maplist(var,Tmp). 
        
:- end_tests(grid_setup).

%%
%% Test making moves
%%

moveTwo([_,o|_]).

:- begin_tests(tictactoe_move, [ setup(initialize_grid) ] ).

test(tictactoe_move1, [ fail ] ) :-
  player_move(x,"1"),grid(Tmp),maplist(var,Tmp).

test(tictactoe_move2) :-
  not(player_move(x,"1")).

test(tictactoe_move3) :-
  player_move(o,"2"),grid(Tmp),moveTwo(Tmp).

:- end_tests(tictactoe_move).

%%
%% Test winning combinations
%%

:- begin_tests(tictactoe_win).

test(tictactoe_win1, [ setup(initialize_grid) ] ) :-
  player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),winner(x).

test(tictactoe_win2, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	winner(x).

test(tictactoe_win3, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(o,"9"),
	winner(x).

test(tictactoe_win4, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(tictactoe_win5, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(x,"7"),player_move(o,"8"),player_move(x,"9"),
	winner(x).

test(tictactoe_win6, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(x,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	winner(x).

test(tictactoe_win7, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(o,"5"),player_move(x,"6"),
	player_move(o,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(tictactoe_win8, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(o,"7"),player_move(x,"8"),player_move(x,"9"),
	winner(x).

test(tictactoe_win9, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	winner(x).

test(tictactoe_win10, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	not(winner(o)).

:- end_tests(tictactoe_win).
