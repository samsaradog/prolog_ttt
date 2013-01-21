%% best_move.pl

:- module(best_move,[draw_game/0,
	                 winner/1,
					 best_move/1,
					 grid_value/4]).

:- use_module(grid).

draw_game :-
	fetch_grid(Current),
	find_moves(Current,Moves),
	length(Moves,Length),
	0 == Length, !.

winner(Player) :-
	fetch_grid(Current),
	find_winner(Player,Current), !.

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

%%%%%%

car([H|_],H).

swap_player(o,x).
swap_player(x,o).

%%% Meta-predicate based on include/3

build_winners([],_,_,[]).
build_winners([Move|T],Player,GridState,Result) :-
	( dup_grid_with_move(Player,GridState,NewGridState,Move),
	  find_winner(Player,NewGridState) -> Result = [Move|NextResult]
	  ; NextResult = Result),
	build_winners(T,Player,GridState,NextResult).

%%%

best_move(Move) :-
	fetch_grid(GridState),
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
	shuffle(["1","3","7","9"],[Move|_]).

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

dup_grid_with_move(P,[G1|T],[P|T],"1") :- var(G1).
dup_grid_with_move(P,[G1,G2|T],[G1,P|T],"2") :- var(G2).
dup_grid_with_move(P,[G1,G2,G3|T],[G1,G2,P|T],"3") :- var(G3).
dup_grid_with_move(P,[G1,G2,G3,G4|T],[G1,G2,G3,P|T],"4") :- var(G4).
dup_grid_with_move(P,[G1,G2,G3,G4,G5|T],[G1,G2,G3,G4,P|T],"5") :- var(G5).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6|T],[G1,G2,G3,G4,G5,P|T],"6") :- var(G6).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7|T],[G1,G2,G3,G4,G5,G6,P|T],"7") :- var(G7).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8|T],[G1,G2,G3,G4,G5,G6,G7,P|T],"8") :- var(G8).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8,G9|T],[G1,G2,G3,G4,G5,G6,G7,G8,P|T],"9") :- var(G9).
