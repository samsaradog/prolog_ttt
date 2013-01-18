%% shuffle.plt

:- use_module(shuffle).

:- begin_tests(shuffle).
/*
test(select_nth0) :-
	select_nth0(2,2,[0,1,2,3],[0,1,3]).
	
test(random_index_failure, [fail]) :-
	random_index([],_).
	
test(random_index_zero) :-
	random_index([1],0).
	
test(random_index) :-
	random_index([1,3,5],X) -> 
	(X==0;X==1;X==2),!.

test(choose_element_fail, [fail]) :-
	choose_element([],_,_).
	
test(choose_element_zero) :-
	choose_element([1],1,[]).
	
test(choose_element) :-
	choose_element([1,2],X,_) ->
	(X==1;X==2),!.
	*/
test(shuffle) :-
	shuffle([1],[1]),!.
	
test(big_shuffle) :-
	shuffle([1,2,3,4],X),
	member(1,X),
	member(2,X),
	member(3,X),
	member(4,X),!.
	
:- end_tests(shuffle).
