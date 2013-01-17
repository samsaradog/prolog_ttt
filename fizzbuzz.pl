

fizzbuzz(X,'fizzbuzz') :- (0 =:= mod(X,15)), !.
fizzbuzz(X,'fizz') :- (0 =:= mod(X,3)), !.
fizzbuzz(X,'buzz') :- (0 =:= mod(X,5)), !.
fizzbuzz(X,X) :- (0 =\= mod(X,5)), (0 =\= mod(X,3)).


:- begin_tests(fizzbuzz_test).

test(fizzbuzz1) :-
  fizzbuzz(1,1).

test(fizzbuzz2) :-
  fizzbuzz(2,2).

test(fizzbuzz3) :-
  fizzbuzz(3,'fizz').

test(fizzbuzz3a) :-
  not(fizzbuzz(3,3)).

test(fizzbuzz5) :-
  fizzbuzz(5,'buzz').

test(fizzbuzz5a) :-
  not(fizzbuzz(5,5)).

test(fizzbuzz6) :-
  fizzbuzz(6,'fizz').
        
test(fizzbuzz10) :-
  fizzbuzz(10,'buzz').
        
test(fizzbuzz15) :-
  fizzbuzz(15,'fizzbuzz').
        
:- end_tests(fizzbuzz_test).
