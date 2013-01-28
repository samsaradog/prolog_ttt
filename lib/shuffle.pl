%% shuffle.pl

:- module(shuffle,[shuffle/2]).

select_nth0(Index,Element,List,NewList) :-
	nth0(Index,List,Element),
	select(Element,List,NewList),!.
	
random_index(List,Index) :-
	length(List,Length),
	Length > 0,
	random(0,Length,Index).

choose_element(List,Element,NewList) :-
	random_index(List,Index),
	select_nth0(Index,Element,List,NewList).
	
shuffle([],[]).
shuffle(List,[Element|NewList]) :-
	choose_element(List,Element,NextList),
	shuffle(NextList,NewList).
