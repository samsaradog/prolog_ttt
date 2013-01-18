%% best_move.pl

:- module(best_move,[best_move/1,
	                 grid_value/4]).

:- use_module(grid).

car([H|_],H).

swap_player(o,x).
swap_player(x,o).

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
