import numpy as np
from enum import Enum
import copy

## Initialize board to standard size. Rows are designated as real columns to make it easier to drop disc
def initializeBoard():
    rows = 6
    return [[0 for _ in range(rows)] for _ in range(7)]

## Transpose board and print. This represents real game board. Rows = 6, Columns = 7
def print_board(board):
    ## transpose board
    outputBoard = np.fliplr(board).T
    for row in outputBoard:
        for element in row:
            ## empty spot
            if (element == 0):
                print("| O | ", end="")
            elif (isPos(element)):
                ## player 1 piece
                print("| + | ", end="")
            else:
                ## player 2 piece
                print("| - | ", end="")
        print("")
        
## Define Player
class WhichPlayer(Enum):
    P1 = 1
    CPU = 2
## Define Status of game
class Status(Enum):
    WIN = 1
    aiWIN = 2
    DRAW = 0
    ONGOING = 3    
    aiONGOING = 4
    
## Find all legal moves of a board
def legalMoves(board):
    arr = []
    for i, row in enumerate(board):
        for j, val in enumerate(row):
            if (val == 0):
                arr.append(i + 1)
                break
    return arr
## Check if the user inputs the correct move
def moveOfString(moves):
    while True:
        user_input = input("Your turn, enter a column to drop your disc --> ")
        ## exit function
        if user_input == "exit":
            return user_input
        try:
        ## test if input is an integer
            user_input = int(user_input)
            if user_input in moves:
                return user_input
            else:
                print("Invalid Move!")
        except ValueError:
            print("That's not an integer!")
            
## Prioritize playing in the middle
def weight(n, disc):
    if(n == 3):
        return 8 * disc
    elif(n == 2 or n == 4):
        return 4 * disc
    elif(n == 1 or n == 5):
        return 2 * disc
    else:
        return disc
## Checks if value is positive
def isPos(n):
    if(n > 0): 
        return True
    else: 
        return False
    
## Check board for horizontal wins
def checkRow(row, disc):
    count = 0
    for i, val in enumerate(row):
        if(count == 4):
            return True
        elif(val == 0):
            count = 0
        elif(isPos(val) == isPos(disc)):
            count+=1
        elif(isPos(val) != isPos(disc)):
            count = 0
    return count == 4

## Check for 4 in a column by iterating vertically
def checkCol(board, i, disc):
    count = 0
    for j in range(0,7):
        val = board[j][i]    
        if(count == 4):
            return True
        elif(val == 0):
            count = 0    
        elif(isPos(val) == isPos(disc)):
            count+=1
        elif(isPos(val) != isPos(disc)):
            count = 0
    return count == 4
## Check board for diagonal wins
def checkDiagonal(board, row, col, disc):
    def checkDirection(delta_row, delta_col):
        arr1, arr2 = [],[]
        i, j = row + delta_row, col + delta_col
        while 0 <= i < len(board) and 0 <= j < len(board[0]):
            arr1.append(board[i][j])
            i += delta_row
            j += delta_col
        # Check in the opposite direction
        i, j = row - delta_row, col - delta_col
        while 0 <= i < len(board) and 0 <= j < len(board[0]):
            arr2.append(board[i][j])
            i -= delta_row
            j -= delta_col
        return arr1, arr2
    (arr1, arr2) = checkDirection(1, 1)
    (arr3, arr4) = checkDirection(-1, 1)
    
    ## stitching arrays in one big array
    arr2 = arr2[::-1] 
    arr2.append(board[row][col])
    arr2.extend(arr1)
    
    arr3 = arr3[::-1] 
    arr3.append(board[row][col])
    arr3.extend(arr4)

    if(checkRow(arr2,disc) or checkRow(arr3,disc)):
        return True
    else:
        return False    
## Make the move on the board
def nextState(status, board, move):
    if(status == Status.aiONGOING):
        disc = -1
    elif(status == Status.ONGOING):
        disc = 1
    row = board[move - 1]  

    for i, val in enumerate(row):
        if(val == 0):
            temp = weight(move - 1, disc)
            board[move - 1][i] = temp
            break
    if(len(legalMoves(board)) == 0):
        return (Status.DRAW,[])
    elif(checkRow(row, disc)):
        if(disc == 1):
            return (Status.WIN,[])
        else:
            return (Status.aiWIN,[])
    elif(checkCol(board, i, disc)):
        if(disc == 1):
            return (Status.WIN,[])
        else:
            return (Status.aiWIN,[])
    elif(checkDiagonal(board, move - 1, i, disc)):
        if(disc == 1):
            return (Status.WIN,[])
        else:
            return (Status.aiWIN,[])
    elif(disc == 1):
        return (Status.aiONGOING,board)
    else:
        return (Status.ONGOING,board)
    
## Check board for diagonal wins
def diagonalValue(board, row, col):
    def checkDirection(delta_row, delta_col):
        arr1, arr2 = [],[]
        i, j = row + delta_row, col + delta_col
        while 0 <= i < len(board) and 0 <= j < len(board[0]):
            arr1.append(board[i][j])
            i += delta_row
            j += delta_col
        # Check in the opposite direction
        i, j = row - delta_row, col - delta_col
        while 0 <= i < len(board) and 0 <= j < len(board[0]):
            arr2.append(board[i][j])
            i -= delta_row
            j -= delta_col
        return arr1, arr2
    (arr1, arr2) = checkDirection(1, 1)
    (arr3, arr4) = checkDirection(-1, 1)
    
    arr2 = arr2[::-1] 
    arr2.append(board[row][col])
    arr2.extend(arr1)
    
    arr3 = arr3[::-1] 
    arr3.append(board[row][col])
    arr3.extend(arr4)
    
    return (arr2, arr3)
def rowValue(lst, score, count, disc):
    for i, val in enumerate(lst):
        if(isPos(val) == isPos(disc)):
            count+=1
        else:
            count = 0
        if(count == 3):
            score += 9 * val
        elif(count == 2):
            score += 3 * val
        elif(count == 1):
            score += val
    return score

def iterate(board, disc):
    score = 0
    for i, row in enumerate(board):
        score+=rowValue(row, 0, 0, disc)
    return score
## AI Player
def estimateValue(status, board):
    if(status == Status.aiWIN):
        return -100
    elif(status == Status.WIN):
        return 100
    elif(status == Status.DRAW):
        return 0.
    else:
        i = 0
        j = 0
        diag1 = 0
        diag2 = 0
        while(i < 6 and j < 7):
            (l, r) = diagonalValue(board,i,j)
            diag1 = max(diag1, rowValue(l, 0,0,1))
            diag2 = max(diag2, rowValue(l, 0,0,-1))
            
            diag1 = max(diag1, rowValue(r, 0,0,1))
            diag2 = max(diag2, rowValue(r, 0,0,-1))
            j+=1
            i+=1
        row1 = iterate(board, 1)
        row2 = iterate(board, -1)    
        col1 = iterate(np.transpose(board), 1)
        col2 = iterate(np.transpose(board), -1)
        return max(diag1, max(row1 , col1)) + min(diag2, min(row2 , col2))
def minimax(status, board, depth):
    moves = legalMoves(board)
    stateLst = []
    for i, move in enumerate(moves):
        board_copy = copy.deepcopy(board)
        status_copy = copy.deepcopy(status)
        stateLst.append(nextState(status_copy, board_copy, move))
    if(depth == 0 or len(moves) == 0):
        return estimateValue(status, board)
    else:
        if(status == Status.aiWIN):
            return -100
        elif(status == Status.WIN):
            return 100
        elif(status == Status.DRAW):
            return 0.
        elif(status == Status.ONGOING):
            base = -1000
            for i, state in enumerate(stateLst):
                b = copy.deepcopy(state[1])
                s = copy.deepcopy(state[0])
                eval = minimax(s, b, depth - 1)
                if(eval > base):
                    base = eval
            return base
        elif(status == Status.aiONGOING):
            base = 1000
            for i, state in enumerate(stateLst):
                b = copy.deepcopy(state[1])
                s = copy.deepcopy(state[0])
                eval = minimax(s, b, depth - 1)
                if(eval < base):
                    base = eval
            return base
        
def bestMove(status, board, moveLst):
    maxValue = 100.
    ans = -1
    for i, move in enumerate(moveLst):
        board_copy = copy.deepcopy(board)
        status_copy = copy.deepcopy(status)
        (s, b) = nextState(status_copy, board_copy, move)
        eval = minimax(s,b,3)
        if(eval < maxValue):
            maxValue = eval
            ans = move
    return ans
    
# Main 
def main():
    board = initializeBoard()
    currentStatus = Status.ONGOING
    print("Hello, Welcome to Austin's Connect4")
    print("Moves are represented by integers from 1 - 7")
    print("Type 'exit to exit the game")
    while(True):
        ## Set up board and legal moves
        print_board(board)
        moves = legalMoves(board)
        
        ## Check if user input is valid
        move = moveOfString(moves)
        if move == "exit":
            break
        
        ## Make the move
        (currentStatus, board) = nextState(currentStatus, board, move)
        if(currentStatus == Status.WIN):
            print("You Win!")
            break
        elif(currentStatus == Status.DRAW):
            print("Its a Draw!")
            break
        print_board(board)
        
        print("AI's turn...", end="")
        moves = legalMoves(board)
        m = bestMove(currentStatus, board, moves)
        (currentStatus, board) = nextState(currentStatus, board, m)
        print("AI makes move " + str(m))
        if(currentStatus == Status.aiWIN):
            print("You Lose!")
            break
        elif(currentStatus == Status.DRAW):
            print("Its a Draw!")
            break

if __name__ == "__main__":
    main()
    
