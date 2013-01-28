%% io.plt

%% mock file for testing

:- module(io, [fetch_input/1,
	          user_message/1,
	          line_break/0]).
	
:- dynamic called/2.
:- dynamic asserted_input/1.


fetch_input(Input) :-
	asserted_input(Input).

user_message(Message) :-
	assert(called(write,Message)).
	
line_break :-
	assert(called(nl,_)).