
## Finds the value of the best move
def minimax(status, depth, disc):
    state = status[0]
    board = status[1]
    moves = legalMoves(board)
    for i, move in enumerate(moves):
        boards = nextState(board,move, disc)
    if(depth == 0):
        return estimateValue(status,disc)
    else:
        if(state == Status.aiWIN):
            return -100.
        elif(state == Status.WIN):
            return 100.
        elif(state == Status.DRAW):
            return 0.
        else:
            if(disc == 1):
                value = -100.
                for i, b in enumerate(boards):
                    max(value, minimax((state, b), depth - 1, disc * -1))
            else:
                value = 100.
                for i, b in enumerate(boards):
                    min(value, minimax((state, b), depth - 1, disc * -1))
            
## Finds the best move
def bestMove(board, moves):
    minV = 100
    for i, move in enumerate(moves):
        evalBoard = minimax(nextState(board, move, -1), 3,1)
        if(evalBoard < minV):
            minV = evalBoard
            ans = move
            
    return ans