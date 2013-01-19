
make_change(X,_) :- X < 0,!,fail.
make_change(X,[X]) :- X > 100,!.

make_change(X,Y) :- 
	Remainder is 100 - X,
	add_quarters(Remainder,Y),!.
	
add_quarters(X,[25|T]) :-
	X > 24,
	Y is X - 25,
	add_quarters(Y,T).

add_quarters(X,Y) :-
	add_dimes(X,Y).
	
add_dimes(X,[10|T]) :-
	X > 9,
	Y is X - 10,
	add_dimes(Y,T).
	
add_dimes(X,Y) :-
	add_nickels(X,Y).
	
add_nickels(X,[5|T]) :-
	X > 4,
	Y is X - 5,
	add_nickels(Y,T).
	
add_nickels(X,Y) :-
	add_pennies(X,Y).
	
add_pennies(0,[]).
add_pennies(X,[1|T]) :-
	Y is X - 1,
	add_pennies(Y,T).

:- begin_tests(coin_changer).

test(less_than_zero) :-
	not(make_change(-1,_)).

test(greater_than_100) :-
	make_change(101,[101]).
	
test(change_for_99) :-
	make_change(99,[1]).
	
test(change_for_98) :-
	make_change(98,[1,1]).
	
test(change_for_94) :-
	make_change(94,[5,1]).
		
test(change_for_89) :-
	make_change(89,[10,1]).
	
test(change_for_84) :-
	make_change(84,[10,5,1]).
	
test(change_for_74) :-
	make_change(74,[25,1]).
	
test(change_for_8) :-
	make_change(8,[25,25,25,10,5,1,1]).
	
:- end_tests(coin_changer).