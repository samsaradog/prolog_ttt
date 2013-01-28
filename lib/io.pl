%% io.pl

:- module(io, [fetch_input/1,
	          user_message/1,
	          line_break/0]).

:- [library(dcg/basics),
    library(dialect/sicstus)].

fetch_input(Input) :-
	read_line(Input).

user_message(Message) :-
	write(Message).
	
line_break :-
	nl.