Note: When minimax has depth 4, it takes 10 seconds per move but is a lot smarter and varies a lot more. Depth 3 is faster but not as smart.

• Instructions for use, describing how a user would interact with your program (how would someone play
your game against a friend? against the AI?)
    To play against AI, go into Referee.re and make lines 49 and 50 look like this
    (HumanPlayer.HumanPlayer(Connect4.Connect4)), 
    (AIPlayer.AIPlayer(Connect4.Connect4));

    To play against a friend, 
    (HumanPlayer.HumanPlayer(Connect4.Connect4)), 
    (HumanPlayer.HumanPlayer(Connect4.Connect4));

    User should input an integer as their move based on the column they want to drop their piece in. If the 
    move is not legal, or if the move is not a string, the program will let the user know and tell it to retry.
    The board is 1-indexed, meaning the first column is a 1, not 0. Player 1 is an O and Player 2 is an X. 
    The goal of the game is to get 4 in a row. 

• An overview of how your program functions, including how all of the pieces fit together
    The game begins with the board being created (rows are represented as columns). Whenever the player enters a 
    a move, the moveOfString will check to see if it is valid by checking with legalMoves. Then, the nextState 
    function is called where the move is done on the board. After placing the disc, nextState will also check for 
    four in a row by calling checkBoard for rows, checkBoard but with mat transposed for columns and then 
    checkDiagonal for diagonals. Depending on what nextState outputs, gameStatus will output something 
    corresponding. Then the board is transposed and printed for the user to see before continuing the cycle. 

• A description of any possible bugs or problems with your program
    Maybe if the board is less then 4 in any direction row, column and diagonal. This is because we hardcoded the 
    program to check for 4 in a row. So if the board is too small, no one will ever win. However, this should never
    happen because the game is Connect4 and the board should never be this small. 

• A list of the people with whom you collaborated
    Austin Liu, Max Boyang, Jeffrey Liang
    TAs: Alex Liang, Sean Yu   

• A description of any extra features you chose to implement
    Chose to implement transpose which switches the rows and columns of the matrix. We created the board internally
    where the rows represents the columns of the board in the game. This made it easier for us to place discs. 
    However, we just transposed the board whenever we output for the user to see better.