# Tic-Tac-Toe in Prolog

Like Lisp, Prolog is another ancient programming language initially developed for AI. It should be no surprise that it has been used to implement searching algorithms, natural language processing, and games!

This is a rapidly-developed, command-line version of the game. It uses an algorithm similar to the minimax algorithm in the C++ and Ruby implementations. 

Setup involves installing a Prolog interpreter on your computer. This project was built using [SWI Prolog](http://www.swi-prolog.org/), only because I couldn't get GNU Prolog to compile on my Mac. More detailed setup and usage instructions are below, and will be revised as development continues.

## Setup Instructions

Install SWI Prolog using the link above. If you don't already have [MacPorts](http://www.macports.org/index.php) installed, go ahead and do that. Then run:

    $ sudo port install swi-prolog

You may get some error messages at this point, and will have to deal with them appropriately. 

## Game Progress

The game is now working from the command line. There is a shell script included with this package that calls swipl with the correct options for running the game. From the command line, in the directory into which you clone this archive, enter:

    ttt

This launches the game and allows you to play.

## Running the Tests

In the directory where you cloned this archive type:

    $ tttest

This runs the tests without going into the SWI REPL. If you want to use the REPL,
enter the following at the command prompt:

    $ swipl
    
This is not exactly like other REPL's you may be familiar with, but close enough for now. If you want to run the tests, first enter:

    ?- ['tictactoe.plt'].

Don't forget the period at the end of the line. This loads the test file, which in turn loads the prolog source file. To actually run the tests, enter:

    ?- run_tests.

Stay tuned for further developments. The adventure continues.

## Sources

There are many resources on the web for learning Prolog. The one I found most useful was a [tutorial](http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/pt_framer.html) by J. R. Fisher at CSU Pomona. You may notice that it contains an implementation of a Tic-Tac-Toe game, which I built on extensively. While there are similarities, such as using a list of unbound variables to represent the game, the search algorithm in the tutorial is different. Dr. (I presume) Fisher used a value assessment for each node that included the number of potential wins available for each player, and only searches the graph to a depth of two. I'm using a plain α/β search with no pruning, and a few goals added to increase efficiency.

## Bonus Material

I have included a fizzbuzz.pl file with a solution to the fizzbuzz kata written in prolog as part of this repo. Running it is similar to the above:

    ?- ['fizzbuzz'].
    ?- run_tests.

Enjoy!
