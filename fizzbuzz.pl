

fizzbuzz(X,Y) :- (0 =:= mod(X,15)), Y = 'fizzbuzz', !.
fizzbuzz(X,Y) :- (0 =:= mod(X,3)), Y = 'fizz', !.
fizzbuzz(X,Y) :- (0 =:= mod(X,5)), Y = 'buzz', !.
fizzbuzz(X,X) :- (0 =\= mod(X,5)), (0 =\= mod(X,3)).


:- begin_tests(fizzbuzz_test).

test(fizzbuzz1) :-
  fizzbuzz(1,1).

test(fizzbuzz2) :-
  fizzbuzz(2,2).

test(fizzbuzz3) :-
  fizzbuzz(3,'fizz').

test(fizzbuzz3a, [fail]) :-
  fizzbuzz(3,3).

test(fizzbuzz5) :-
  fizzbuzz(5,'buzz').

test(fizzbuzz5a, [fail]) :-
  fizzbuzz(5,5).

test(fizzbuzz6) :-
  fizzbuzz(6,'fizz').
        
test(fizzbuzz10) :-
  fizzbuzz(10,'buzz').
        
test(fizzbuzz15) :-
  fizzbuzz(15,'fizzbuzz').
        
:- end_tests(fizzbuzz_test).
