:- include(tictactoe).
:- use_module(library(apply)).

allVar(List) :- maplist(var, List).

someVar([_A,o,_B]).

:- begin_tests(tictactoe_setup, [setup(initializeGrid)] ).

test(tictactoe3, [ setup(someVar(Tmp)), fail ] ) :-
  allVar(Tmp). 
        
test(tictactoe4) :-
  grid(Tmp),allVar(Tmp). 
        
:- end_tests(tictactoe_setup).

moveTwo([_,o|_]).

:- begin_tests(tictactoe_move, [ setup(initializeGrid) ] ).

test(tictactoe_move1, [ fail ] ) :-
  playerMove(x,1),grid(Tmp),allVar(Tmp).

test(tictactoe_move2) :-
  playerMove(o,2),grid(Tmp),moveTwo(Tmp).

test(tictactoe_move3, [ fail ] ) :-
  playerMove(x,1),move(x,1).

:- end_tests(tictactoe_move).

:- begin_tests(tictactoe_win, [ setup(initializeGrid) ] ).

test(tictactoe_win1) :-
  playerMove(x,1),playerMove(x,2),playerMove(x,3),winner(x).

:- end_tests(tictactoe_win).
