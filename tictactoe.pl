%% tictactoe.pl

:- [library(dcg/basics),
    library(dialect/sicstus)].

:- use_module(grid).
:- use_module(shuffle).
:- use_module(best_move).
:- use_module(view).

%%%%%%%%%%%

check_draw_game :-
	draw_game,
	write('draw game'),nl,nl.
	
check_winner :- 
	winner(o), 
	write('the computer has won!'),nl,nl.
	
%%%%%%%%%%%

first_move(0) :-
    nl,write('human goes first'),nl,nl.

first_move(1) :- 
	computer_move,
	nl,write('computer goes first'),nl,nl.

%%%%%%%%%%%

computer_move :- 
	best_move(X),
	player_move(o,X).

human_move(Move) :-
	nl,write('please choose a square (1-9):'),nl,
	read_line(Move),nl,
	move_available(Move), !.
	
human_move(Move) :-
	write('the square you selected is not available'),nl,
	human_move(Move).

%%%%%%%%%%%

play(human) :-
  human_move(Move),
  player_move(x,Move),
  not(check_draw_game), !,
  play(computer).

play(computer) :-
  computer_move,
  not(check_winner), !,
  not(check_draw_game),
  show_game,
  play(human).

%%%%%%%%%%%

new_game :-
  initialize_grid,
  R is random(2),
  write('randomly selecting first player'),nl,
  first_move(R),
  show_game,
  play(human).

show_game :-
  fetch_grid(GameState),
  display_grid(GameState,[1,2,3,4,5,6,7,8,9]).

%%%%%%%%%%%

good_answer("Y").
good_answer("y").
good_answer("N").
good_answer("n").

ask_to_play_again(Answer) :-
	write('would you like to play again? (y/n):'),nl,
	read_line(Answer),nl,
	good_answer(Answer),!.

ask_to_play_again(Answer) :-
	nl,write('please enter y or n'),nl,nl,
	ask_to_play_again(Answer).

%%%%%%%%%%%

done :-
	show_game,nl,
	ask_to_play_again(Answer),!,
	( Answer == "y"; Answer == "Y"),!,
	ttt.

ttt :-
	new_game.
	
ttt :-
	done.