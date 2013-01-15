# Tic-Tac-Toe in Prolog

Like Lisp, Prolog is another ancient programming language initially developed for AI. It should be no surprise that it has been used to implement searching algorithms, natural language processing, and games!

This is a rapidly-developed, command-line version of the game. It uses the same minimax algorithm as was in the C++ and Ruby implementations, obviously expressed differently. Setup involves getting a Prolog interpreter set up on your computer. This was built using [SWI Prolog](http://www.swi-prolog.org/), only because I couldn't get GNU Prolog to compile on my Mac. More detailed usage instructions are below, and will be revised as development continues.

## Setup Instructions

Install SWI Prolog using the link above. Move into the directory where you cloned this archive and type:

    $ swipl
    
This will start a REPL-like service for Prolog. This is not exactly like other REPL's you may be familiar with, but close enough for now. At the prompt enter:

    ?- ['tictactoe.plt'].

Don't forget the period at the end of the line. This loads the test file, which in turn loads the prolog source file. To run the tests, enter:

    ?- run_tests.

Stay tuned for further developments. The adventure continues.

## Bonus Material

There is a fizzbuzz.pl file with a solution to the fizzbuzz kata in prolog for your enjoyment and edification. Running it is similar to the above:

    ?- ['fizzbuzz'].
    ?- run_tests.

Enjoy!
