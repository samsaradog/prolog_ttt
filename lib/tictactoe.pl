%% tictactoe.pl

:- use_module(grid).
:- use_module(shuffle).
:- use_module(best_move).
:- use_module(view).
:- use_module(io).

%%%%%%%%%%%

check_draw_game :-
	draw_game,
	user_message('draw game'),line_break,line_break.
	
check_winner :- 
	winner(o), 
	user_message('the computer has won!'),line_break,line_break.
	
%%%%%%%%%%%

first_move(0) :-
    line_break,user_message('human goes first'),line_break,line_break.

first_move(1) :- 
	computer_move,
	line_break,user_message('computer goes first'),line_break,line_break.

%%%%%%%%%%%

computer_move :- 
	best_move(X),
	player_move(o,X).

human_move(Move) :-
	line_break,user_message('please choose a square (1-9):'),line_break,
	fetch_input(Move),line_break,
	move_available(Move), !.
	
human_move(Move) :-
	user_message('the square you selected is not available'),line_break,
	human_move(Move).

%%%%%%%%%%%

play(human) :-
  human_move(Move),
  player_move(x,Move),
  \+check_draw_game, !,
  play(computer).

play(computer) :-
  computer_move,
  \+check_winner, !,
  \+check_draw_game,
  show_game,
  play(human).

%%%%%%%%%%%

new_game :-
  initialize_grid,
  R is random(2),
  user_message('randomly selecting first player'),line_break,
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
	user_message('would you like to play again? (y/n):'),line_break,
	fetch_input(Answer),line_break,
	good_answer(Answer),!.

ask_to_play_again(Answer) :-
	line_break,user_message('please enter y or n'),line_break,line_break,
	ask_to_play_again(Answer).

%%%%%%%%%%%

done :-
	show_game,line_break,
	ask_to_play_again(Answer),!,
	( Answer == "y"; Answer == "Y"),!,
	ttt.

ttt :-
	new_game.
	
ttt :-
	done.