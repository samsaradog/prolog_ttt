
make_change(X,_) :- X < 0,!,fail.
make_change(X,[X]) :- X > 100,!.

make_change(X,Y) :- 
	Remainder is 100 - X,
	make_change(Remainder,[25,10,5,1],Y),!.
	
make_change(0,_,[]).
make_change(X,[FirstCoin|OtherCoins],[FirstCoin|List]) :-
	X >= FirstCoin,
	Y is X - FirstCoin,
	make_change(Y,[FirstCoin|OtherCoins],List).
	
make_change(X,[_FirstCoin|OtherCoins],List) :-
	make_change(X,OtherCoins,List).
	
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