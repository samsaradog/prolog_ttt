:- include(tictactoe).
:- use_module(library(apply)).

all_var(List) :- maplist(var, List).

someVar([_A,o,_B]).

:- begin_tests(extract_move).

test(extract_move1) :-
	extract_move([0,-1,-1],[1,2,3],1).

test(extract_move2) :-
	extract_move([0,1,-1],[1,2,3],2).

test(extract_move3) :-
	extract_move([0,-1,1],[1,2,3],3).

:- end_tests(extract_move).

:- begin_tests(shuffle).

test(select_nth0) :-
	select_nth0(2,2,[0,1,2,3],[0,1,3]).
	
test(random_index_failure, [fail]) :-
	random_index([],_).
	
test(random_index_zero) :-
	random_index([1],0).
	
test(random_index) :-
	random_index([1,3,5],X) -> 
	(X==0;X==1;X==2),!.

test(choose_element_fail, [fail]) :-
	choose_element([],_,_).
	
test(choose_element_zero) :-
	choose_element([1],1,[]).
	
test(choose_element) :-
	choose_element([1,2],X,_) ->
	(X==1;X==2),!.
	
test(shuffle) :-
	shuffle([1],[1]),!.
	
test(big_shuffle) :-
	shuffle([1,2,3,4],X),
	member(1,X),
	member(2,X),
	member(3,X),
	member(4,X),!.
	
:- end_tests(shuffle).

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

%%
%% Test setting up the board
%%

:- begin_tests(tictactoe_setup, [setup(initialize_grid)] ).

test(tictactoe3, [ setup(someVar(Tmp)), fail ] ) :-
  all_var(Tmp). 
        
test(tictactoe4) :-
  grid(Tmp),all_var(Tmp). 
        
:- end_tests(tictactoe_setup).

%%
%% Test making moves
%%

moveTwo([_,o|_]).

:- begin_tests(tictactoe_move, [ setup(initialize_grid) ] ).

test(tictactoe_move1, [ fail ] ) :-
  player_move(x,"1"),grid(Tmp),all_var(Tmp).

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

%%
%% Test finding the best move
%%

:- begin_tests(tictactoe_best_move).

%% Fail if the grid is full

test(tictactoe_best_move0, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	not(best_move(_)).

%% Last empty space on the grid

test(tictactoe_best_move1, [ setup(initialize_grid) ] ) :-
	player_move(o,"1"),player_move(o,"2"),player_move(x,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),
	best_move("9").

test(tictactoe_best_move2, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	best_move("1").

test(tictactoe_best_move3, [ setup(initialize_grid), fail ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(x,"6"),
	player_move(x,"7"),player_move(x,"8"),player_move(o,"9"),
	best_move("9").
	
%% Make a winning move

test(tictactoe_best_move4, [ setup(initialize_grid) ] ) :-
	player_move(x,"1"),player_move(o,"2"),player_move(o,"3"),
	player_move(o,"4"),player_move(x,"5"),player_move(o,"6"),
	player_move(x,"7"),
	best_move("9").

test(tictactoe_best_move5, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),                
	                
	best_move("1").

test(tictactoe_best_move6, [ setup(initialize_grid) ] ) :-
	                   player_move(o,"2"),player_move(o,"3"),
	player_move(x,"4"),player_move(x,"5"),player_move(o,"6"),                

	best_move(X) ->(X=="1";X=="9"),!.
	
test(tictactoe_best_move7, [ setup(initialize_grid) ] ) :-
	                player_move(x,"2"),player_move(o,"3"),
	                player_move(x,"5"),player_move(o,"6"),                

	best_move("9").

%% Block a winning move

test(tictactoe_best_move8, [ setup(initialize_grid) ] ) :-
	                   player_move(x,"2"),player_move(x,"3"),
	player_move(x,"4"),player_move(o,"5"),player_move(o,"6"),
	player_move(o,"7"),
	best_move("1").

test(tictactoe_best_move9, [ setup(initialize_grid) ] ) :-
	                                   player_move(o,"3"),
	                player_move(x,"5"),player_move(x,"6"),
	                                
	best_move("4").

%% Pick any position

test(tictactoe_best_move10, [ setup(initialize_grid) ] ) :-
	                   player_move(x,"2"),player_move(o,"3"),
	                   player_move(o,"5"),player_move(x,"6"),
	player_move(x,"7"),                                
	best_move(X) -> (X=="1";X=="4";X=="8";X=="9"),!.
	
%% Don't pick a corner 
	
test(tictactoe_best_move11, [ setup(initialize_grid) ] ) :-
	                                      player_move(x,"3"),
	                   player_move(o,"5"),                
	player_move(x,"7"),
	best_move(X), "9" =\= X, "1" =\= X.

%% If the middle is available, take it.

test(tictactoe_best_move12, [ setup(initialize_grid) ] ) :-
	best_move("5").


%% If the middle is taken, take a corner.

test(tictactoe_best_move13, [ setup(initialize_grid) ] ) :-
	player_move(x,"5"),
	best_move(X) -> (X=="1";X=="3";X=="6";X=="9"),!.


:- end_tests(tictactoe_best_move).
