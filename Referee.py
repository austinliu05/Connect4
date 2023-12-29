from Connect4 import Connect4
from AIPlayer import AIPlayer

def main():
    game = Connect4()
    ai_player = AIPlayer()
    
    board = game.initializeBoard()
    print("Hello, Welcome to Austin's Connect4")
    print("Moves are represented by integers from 1 - 7")
    print("Type 'exit to exit the game")
    while(True):
        ## Set up board and legal moves
        game.print_board(board)
        moves = game.legalMoves(board)
        
        ## Check if user input is valid
        move = moveOfString(moves)
        if move == "exit":
            break
        
        ## Make the move
        (currentStatus, board) = nextState(board, move, 1)
        
        ## AI Players Turn
        moves = legalMoves(board)
        print("Now its the AI Player's turn...")
        (currentStatus, board) = nextState(board, bestMove(board, moves), -1)
if __name__ == "__main__":
    main()
    
