# Tic-Tac-Toe in Prolog

Like Lisp, Prolog is another ancient programming language initially developed for AI. It should be no surprise that it has been used to implement searching algorithms, natural language processing, and games!

This is a rapidly-developed, command-line version of the game. It uses the same minimax algorithm as was in the C++ and Ruby implementations, obviously expressed differently. Setup involves getting a Prolog interpreter set up on your computer. This was built using [SWI Prolog](http://www.swi-prolog.org/), only because I couldn't get GNU Prolog to compile on my Mac. More detailed usage instructions are below, and will be revised as development continues.

## Setup Instructions

Install SWI Prolog using the link above. If you don't already have [MacPorts](http://www.macports.org/index.php) installed, go ahead and do that. Then run:

    $ sudo port install swi-prolog

You may get some error messages at this point, and will have to deal with them appropriately. Most likely though, even with the errors, you can probably just move into the directory where you cloned this archive and type:

    $ swipl
    
This will start a REPL-like service for Prolog. This is not exactly like other REPL's you may be familiar with, but close enough for now. At the prompt enter:

    ?- ['tictactoe.plt'].

Don't forget the period at the end of the line. This loads the test file, which in turn loads the prolog source file. To run the tests, enter:

    ?- run_tests.

Stay tuned for further developments. The adventure continues.

## Game Progress

The game search algorithm is now working from within the repl. There is not yet a check on whether you can continue playing. This results in an error if the human enters the last move on the grid. To start a new game, enter:

    ?- newGame.

This will display the game board. To make a move, enter, for example:

    ?- move(2).

Be sure not to forget the period at the end of the command. The program will announce a winner, but not a draw game. Next step is to get a more friendly user interface running from a command line rather than inside the repl.

## Sources

There are many resources on the web for learning Prolog. The one I found most useful was a [tutorial](http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/pt_framer.html) by J. R. Fisher at CSU Pomona. You may notice that it contains an implementation of a Tic-Tac-Toe game, which I built on
extensively. While there are similarities, such as using a list of unbound variables
to represent the game, the search algorithm in the tutorial is different. Dr. (I presume) Fisher used a value assessment for each node that included the number of potential wins available for each player, and only searches the graph to a depth of two. If I use a minimax algorithm, I'll use the same pruning algorithm as in other implementations, meaning to search the entire pruned graph to find the best move. Then again, I may revert to a rule-based system like I used in my first c++ implementation.

## Bonus Material

There is a fizzbuzz.pl file with a solution to the fizzbuzz kata in prolog for your pleasure and edification. Running it is similar to the above:

    ?- ['fizzbuzz'].
    ?- run_tests.

Enjoy!
