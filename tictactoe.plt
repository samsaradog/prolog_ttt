:- include(tictactoe).
:- use_module(library(apply)).

allVar(List) :- maplist(var, List).

someVar([_A,o,_B]).

:- begin_tests(tictactoe_test).

test(tictactoe3, [ setup(someVar(Tmp)), fail ] ) :-
  allVar(Tmp). 
        
test(tictactoe4, [ setup(initializeGrid(Tmp)) ] ) :-
  allVar(Tmp). 
        
:- end_tests(tictactoe_test).
