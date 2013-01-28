%% tictactoe.plt

:- include(lib/tictactoe).

:- load_files('spec/mocks/io.pl',[redefine_module(true)]).

%%%%%%

clean_assertions :-
	io:retractall(called(_,_)),
	grid:initialize_grid.

make_draw :-
	fill_grid(["1","2","3","4","5","6","7","8","9"]).
	
make_win :-
	fill_grid(["1","2","3"]).
	
fill_grid([]).
fill_grid([H|T]) :-
	grid:player_move(o,H),
	fill_grid(T).
	
%%%%%%

:- begin_tests(game_checks,[setup(clean_assertions)]).

test(initial_game) :-
	\+tictactoe:check_draw_game,
	\+tictactoe:check_winner.
	
test(draw_game,[setup(make_draw),
                cleanup(clean_assertions)]) :-
	tictactoe:check_draw_game,
	io:called(write,'draw game').
	
test(winning_game,[setup(make_win),
	               cleanup(clean_assertions)]) :-
	tictactoe:check_winner,
	io:called(write,'the computer has won!').
	
:- end_tests(game_checks).

%%%%%%

fetch_available_moves(Moves) :-
	grid:fetch_grid(Grid),
	grid:find_moves(Grid,Moves).
	
first_move_result(Selector,Moves) :-
	tictactoe:first_move(Selector),
	fetch_available_moves(Moves).

:- begin_tests(first_move,[setup(clean_assertions)]).

test(human_first_move,[cleanup(clean_assertions)]) :-
	first_move_result(0,Moves),
	length(Moves,9),
	io:called(write,'human goes first'),!.
	
test(computer_first_move,[cleanup(clean_assertions)]) :-
	first_move_result(1,Moves),
	length(Moves,8),
	io:called(write,'computer goes first'),!.

:- end_tests(first_move).

%%%%%%

:- begin_tests(computer_move,[setup(clean_assertions)]).

test(computer_move) :-
	tictactoe:computer_move,
	fetch_available_moves(Moves),
	length(Moves,8),!.

:- end_tests(computer_move).

%%%%%%

:- dynamic io:asserted_input/1.

:- begin_tests(human_move,[setup(clean_assertions)]).

test(good_human_move) :-
	assert(io:asserted_input("1")),
	tictactoe:human_move(Move),
	Move == "1",
	io:called(write,'please choose a square (1-9):'),
	retract(io:asserted_input(_)),!.

:- end_tests(human_move).