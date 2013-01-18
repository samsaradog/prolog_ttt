%% tictactoe.pl

:- [library(dcg/basics),
    library(dialect/sicstus)].

%% Any similarity to lisp is intentional

car([H|_],Element) :- Element = H.

%% Used for shuffling moves

:- use_module(shuffle).

/*
select_nth0(Index,Element,List,NewList) :-
	nth0(Index,List,Element),
	select(Element,List,NewList),!.
	
random_index(List,Index) :-
	length(List,Length),
	Length > 0,
	random(0,Length,Index).

choose_element(List,Element,NewList) :-
	random_index(List,Index),
	select_nth0(Index,Element,List,NewList).
	
shuffle([],[]).
shuffle(List,[Element|NewList]) :-
	choose_element(List,Element,NextList),
	shuffle(NextList,NewList).
*/	
%% Grid goals
/*
initialize_grid :-
  retractall(grid(_)),
  assert(grid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9])).

match_move_to_grid(Player, [X|_],"1") :- var(X), X=Player.
match_move_to_grid(Player, [_,X|_],"2") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,X|_],"3") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,X|_],"4") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,X|_],"5") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,X|_],"6") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,X|_],"7") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,_,X|_],"8") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,_,_,X|_],"9") :- var(X), X=Player.

dup_grid_with_move(P,[G1|T],[P|T],"1") :- var(G1).
dup_grid_with_move(P,[G1,G2|T],[G1,P|T],"2") :- var(G2).
dup_grid_with_move(P,[G1,G2,G3|T],[G1,G2,P|T],"3") :- var(G3).
dup_grid_with_move(P,[G1,G2,G3,G4|T],[G1,G2,G3,P|T],"4") :- var(G4).
dup_grid_with_move(P,[G1,G2,G3,G4,G5|T],[G1,G2,G3,G4,P|T],"5") :- var(G5).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6|T],[G1,G2,G3,G4,G5,P|T],"6") :- var(G6).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7|T],[G1,G2,G3,G4,G5,G6,P|T],"7") :- var(G7).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8|T],[G1,G2,G3,G4,G5,G6,G7,P|T],"8") :- var(G8).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8,G9|T],[G1,G2,G3,G4,G5,G6,G7,G8,P|T],"9") :- var(G9).

player_move(Player,Position) :-
  grid(Current),
  match_move_to_grid(Player,Current,Position),
  retract(grid(_)),
  assert(grid(Current)), !. 

%% Rows
find_winner(Player, [G1,G2,G3, _, _, _, _, _, _]) :- G1 == Player,G2 == Player,G3 == Player.
find_winner(Player, [ _, _, _,G4,G5,G6, _, _, _]) :- G4 == Player,G5 == Player,G6 == Player.
find_winner(Player, [ _, _, _, _, _, _,G7,G8,G9]) :- G7 == Player,G8 == Player,G9 == Player.

%% Columns
find_winner(Player, [G1, _, _,G4, _, _,G7, _, _]) :- G1 == Player,G4 == Player,G7 == Player.
find_winner(Player, [ _,G2, _, _,G5, _, _,G8, _]) :- G2 == Player,G5 == Player,G8 == Player.
find_winner(Player, [ _, _,G3, _, _,G6, _, _,G9]) :- G3 == Player,G6 == Player,G9 == Player.

%% Diagonals
find_winner(Player, [G1, _, _, _,G5, _, _, _,G9]) :- G1 == Player,G5 == Player,G9 == Player.
find_winner(Player, [ _, _,G3, _,G5, _,G7, _, _]) :- G3 == Player,G5 == Player,G7 == Player.

find_moves(GridState,Moves) :- 
	findall(X,match_move_to_grid(o,GridState,X),OrderedMoves),
	shuffle(OrderedMoves,Moves).

winner(Player) :-
  grid(Current),
  find_winner(Player,Current), !.
*/

grid_positions([1,2,3,4,5,6,7,8,9]).

view_cell(X,Y) :- var(X), write(Y).
view_cell(X,_Y) :- \+var(X),write(X).

swap_player(o,x).
swap_player(x,o).

best_move(Move) :-
	grid(GridState),
	find_moves(GridState,Moves),
	find_best_move(o,Move,Moves,GridState), !.

%% when the list of available moves is empty, fail

find_best_move(_Player,_Move,Moves,_GridState) :- 
	length(Moves,0), !, fail.
	
%% pick the middle if available.

find_best_move(_Player,Move,Moves,_GridState) :-
	member("5",Moves),Move="5".
	
%% pick a corner if only the middle is taken

find_best_move(_Player,Move,Moves,_GridState) :-
	length(Moves,8),
	not(member("5",Moves)),
	shuffle(["1","3","7","9"],[H|_]),
	Move=H.

%% when best move is the last one on the grid.

find_best_move(_Player,Move,Moves,_GridState) :- 
	length(Moves,1),
	car(Moves,Move).

%% when best move is the winning move

find_best_move(Player,Move,Moves,GridState) :- 
	build_winners(Moves,Player,GridState,Result), 
	car(Result,Move).
	
%% when best move blocks a winning move
	
find_best_move(Player,Move,Moves,GridState) :-
	swap_player(Player,OtherPlayer),
	build_winners(Moves,OtherPlayer,GridState,Result),
	car(Result,Move).
	
%% When we have to search for the best move. This is only called
%% by the computer at the root. From here down we look at grid values

find_best_move(Player,Move,Moves,GridState) :-
	build_values(Player,GridState,Moves,Values),
	extract_move(Values,Moves,Move).

extract_move([ValueHead|ValueTail],[MoveHead|MoveTail],Result) :-
	extract_move(ValueTail,ValueHead,MoveTail,MoveHead,Result).
	
extract_move([],_,[],Move,Move).
extract_move([ValueHead|ValueTail],Value,[MoveHead|MoveTail],Move,Result) :-
	ValueHead > Value -> extract_move(ValueTail,ValueHead,MoveTail,MoveHead,Result)
	; extract_move(ValueTail,Value,MoveTail,Move,Result).
	
%% Meta-predicate based on include/3
	
build_winners([],_,_,[]).
build_winners([Move|T],Player,GridState,Result) :-
	( dup_grid_with_move(Player,GridState,NewGridState,Move),
	  find_winner(Player,NewGridState) -> Result = [Move|NextResult]
	  ; NextResult = Result),
	build_winners(T,Player,GridState,NextResult).
	
%% Derives value for current grid

grid_value(_,_Moves,GridState,-1) :- find_winner(x,GridState),!.
grid_value(_,_Moves,GridState, 1) :- find_winner(o,GridState),!.
grid_value(_,Moves,_GridState, 0) :- 
	length(Moves,Length),
	0 == Length,!.

grid_value(Player,Moves,GridState,Value) :-
	swap_player(Player,OtherPlayer),
	build_values(OtherPlayer,GridState,Moves,Values),
	extract_value(OtherPlayer,Values,Value).

build_values(_Player,_GridState,[],[]).
build_values(Player,GridState,[Move|T],Values) :-
	( dup_grid_with_move(Player,GridState,NewGridState,Move),
	  find_moves(NewGridState,Moves),
	  grid_value(Player,Moves,NewGridState,Value) -> Values = [Value|NextValues]
	  ; NextValues = Values),
	build_values(Player,GridState,T,NextValues).
	
extract_value(x,Values,Value) :- min_list(Values,Value).
extract_value(o,Values,Value) :- max_list(Values,Value).

%%
%% Sugar goals for human players
%%

draw_game :-
	grid(Current),
	find_moves(Current,Moves),
	length(Moves,Length),
	0 == Length, !,
	write('draw game'),nl,nl.

check_winner :- 
	winner(o), 
	write('the computer has won!'),nl,nl.

display_grid([],[]).
display_grid([G1,G2,G3|GT],[P1,P2,P3|PT]) :-
  write(' '), view_cell(G1,P1),
  write(' | '),view_cell(G2,P2),
  write(' | '),view_cell(G3,P3),nl,
  write('-----------'),nl,
  display_grid(GT,PT), !.

first_move(0) :-
    nl,write('human goes first'),nl,nl.

first_move(1) :- 
	computer_move,
	nl,write('computer goes first'),nl,nl.

computer_move :- 
	best_move(X),
	player_move(o,X).
	
good_move(Move) :-
	grid(Current),
	match_move_to_grid(x,Current,Move).
	
human_move(Move) :-
	nl,write('please choose a square (1-9):'),nl,
	read_line(Move),nl,
	good_move(Move), !.
	
human_move(Move) :-
	write('the square you selected is not available'),nl,
	human_move(Move).

play(human) :-
  human_move(Move),
  player_move(x,Move),
  not(draw_game), !,
  play(computer).

play(computer) :-
  computer_move,
  not(check_winner), !,
  not(draw_game),
  show_game,
  play(human).

new_game :-
  initialize_grid,
  R is random(2),
  first_move(R),
  show_game,
  play(human).

show_game :-
  grid_positions(Positions),
  grid(GameState),
  display_grid(GameState,Positions).

good_answer("Y").
good_answer("y").
good_answer("N").
good_answer("n").

ask_to_play_again(Answer) :-
	write('would you like to play again? (y/n):'),nl,
	read_line(Answer),
	good_answer(Answer),!.

ask_to_play_again(Answer) :-
	nl,write('please enter y or n'),nl,nl,
	ask_to_play_again(Answer).

done :-
	show_game,nl,
	ask_to_play_again(Answer),!,
	( Answer == "y"; Answer == "Y"),!,
	ttt.

ttt :-
	new_game.
	
ttt :-
	done.