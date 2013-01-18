%% grid.pl

:- module(grid, [initialize_grid/0,
	             player_move/2,
	             find_moves/2,
	             find_winner/2,
	             winner/1,
	             draw_game/0,
	             fetch_grid/1,
	             dup_grid_with_move/4,
	             move_available/1]).

:- use_module(shuffle).

initialize_grid :-
  retractall(grid(_)),
  assert(grid([_C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8,_C9])).

player_move(Player,Position) :-
  grid(Current),
  match_move_to_grid(Player,Current,Position),
  retract(grid(_)),
  assert(grid(Current)), !. 

find_moves(GridState,Moves) :- 
	findall(X,match_move_to_grid(o,GridState,X),OrderedMoves),
	shuffle(OrderedMoves,Moves).

winner(Player) :-
	grid(Current),
	find_winner(Player,Current), !.
	
draw_game :-
	grid(Current),
	find_moves(Current,Moves),
	length(Moves,Length),
	0 == Length, !.
	
fetch_grid(Grid) :- grid(Grid).

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

dup_grid_with_move(P,[G1|T],[P|T],"1") :- var(G1).
dup_grid_with_move(P,[G1,G2|T],[G1,P|T],"2") :- var(G2).
dup_grid_with_move(P,[G1,G2,G3|T],[G1,G2,P|T],"3") :- var(G3).
dup_grid_with_move(P,[G1,G2,G3,G4|T],[G1,G2,G3,P|T],"4") :- var(G4).
dup_grid_with_move(P,[G1,G2,G3,G4,G5|T],[G1,G2,G3,G4,P|T],"5") :- var(G5).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6|T],[G1,G2,G3,G4,G5,P|T],"6") :- var(G6).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7|T],[G1,G2,G3,G4,G5,G6,P|T],"7") :- var(G7).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8|T],[G1,G2,G3,G4,G5,G6,G7,P|T],"8") :- var(G8).
dup_grid_with_move(P,[G1,G2,G3,G4,G5,G6,G7,G8,G9|T],[G1,G2,G3,G4,G5,G6,G7,G8,P|T],"9") :- var(G9).

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
