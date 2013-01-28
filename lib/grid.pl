%% grid.pl

:- module(grid, [initialize_grid/0,
                 fetch_grid/1,
	             player_move/2,
	             find_moves/2,
	             move_available/1]).

:- use_module(shuffle).

initialize_grid :-
  retractall(grid(_)),
  assert(grid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9])).

fetch_grid(Grid) :- grid(Grid).

player_move(Player,Position) :-
  grid(Current),
  match_move_to_grid(Player,Current,Position),
  retract(grid(_)),
  assert(grid(Current)), !. 

find_moves(GridState,Moves) :- 
	findall(X,match_move_to_grid(o,GridState,X),OrderedMoves),
	shuffle(OrderedMoves,Moves).

move_available(Move) :-
	grid(Current),
	match_move_to_grid(_,Current,Move).

match_move_to_grid(Player, [X|_],"1") :- var(X), X=Player.
match_move_to_grid(Player, [_,X|_],"2") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,X|_],"3") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,X|_],"4") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,X|_],"5") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,X|_],"6") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,X|_],"7") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,_,X|_],"8") :- var(X), X=Player.
match_move_to_grid(Player, [_,_,_,_,_,_,_,_,X|_],"9") :- var(X), X=Player.

