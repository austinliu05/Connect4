
import './Game.css'
import { useState } from 'react'
import { ref, set } from 'firebase/database';
import {database} from '../firebaseConfig'

const Game = ({ onClose, record}) => {
    // hooks
    const ROWS = 6;
    const COLUMNS = 7;
    const [hasWon, setHasWon] = useState(false);
    const [hasLost, setHasLost] = useState(false);
    const [hasDraw, setHasDraw] = useState(false);
    const [isGameActive, setIsGameActive] = useState(true);
    const [isInvalid, setInvalid] = useState(false);
    const [isPlayerTurn, setIsPlayerTurn] = useState(true);
    const [recent] = useState(false);
    // creating board
    const generateGrid = () => {
        const grid = [];
        for (let row = 0; row < ROWS; row++) {
            const currentRow = [];
            for (let col = 0; col < COLUMNS; col++) {
                currentRow.push({ id: `row${row}-col${col}`, row, col, isClicked: false, AIPlace: false });
            }
            grid.push(currentRow);
        }
        return grid;
    };
    // creating visual for the board (different colors)
    const [gridData, setGridData] = useState(generateGrid());
    const Grid = ({ gridData, onButtonClick }) => {
        return (
            <div className="grid">
                {gridData.map((row, rowIndex) => (
                    <div key={rowIndex} className="grid-row">
                        {row.map(cell => (
                            <button
                                key={cell.id}
                                className={`${cell.isClicked ? 'clicked' : cell.recent ? 'recent' : cell.AIPlace ? 'placed' : !isGameActive || !isPlayerTurn ? 'no-hover' : ''}`}
                                onClick={() => onButtonClick(cell.row, cell.col, cell.id)}>
                                {/* Display game state here */}
                            </button>
                        ))}
                    </div>
                ))}
            </div>
        );
    };
    // if button is clicked by the user
    const handleButtonClick = (row, col, id) => {
        console.log(`Button clicked at row ${row}, column ${col}`);
        if (!isGameActive || !isPlayerTurn) {
            return
        }
        if (gridData[row][col].isClicked || gridData[row][col].AIPlace) {
            setInvalid(true)
            return
        } else if (row < 5 && !gridData[row + 1][col].isClicked && !gridData[row + 1][col].AIPlace) {
            setInvalid(true)
            return
        }
        setInvalid(false)
        // Update the grid data to reflect the clicked state
        const newGridData = gridData.map(rowData => rowData.map(cell => {
            if (cell.id === id) {
                return { ...cell, isClicked: true }; // Return a new object for the clicked cell
            }
            return { ...cell }; // Return a new object for other cells
        }));
        // modifyng board with user input
        setGridData(newGridData);
        let board = []
        for (let i = 0; i < 6; i++) {
            let currRow = [];
            for (let j = 0; j < 7; j++) {
                let val = 0
                if (newGridData[i][j].isClicked) {
                    val = 1
                } else if (newGridData[i][j].AIPlace) {
                    val = -1
                }
                if (j === 3) {
                    val = val * 8
                } else if (j === 2 || j === 4) {
                    val = val * 4
                } else if (j === 1 || j === 5) {
                    val = val * 2
                }
                currRow.push(val)
            }
            board.push(currRow)
        }
        let data = { "x": row, "y": col, "board": board }
        console.log(data)
        sendData(data)
        setIsPlayerTurn(false);
    };
    // fetching data from backend flask 
    const [data, setData] = useState([{}])
    const sendData = (data) => {
        // https://apoxie.pythonanywhere.com/Connect4
        fetch("https://apoxie.pythonanywhere.com/connect4", {
            method: "POST",
            cache: "no-store",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(data)
        })
            .then(
                res => res.json())
            .then(responseData => {
                console.log("Server Response:", responseData);
                responseData = JSON.parse(responseData)
                // checks for game state
                if (responseData["status"] === 'WIN') {
                    setHasWon(true)
                    setIsGameActive(false);
                    updateRecord(record.Wins, record.Losses + 1, record.Ties);
                } else if (responseData["status"] === 'LOSE') {
                    setHasLost(true)
                    setIsGameActive(false);
                    updateRecord(record.Wins + 1, record.Losses, record.Ties);
                } else if (responseData["status"] === 'DRAW') {
                    setHasDraw(true)
                    setIsGameActive(false);
                    updateRecord(record.Wins, record.Losses, record.Ties + 1);
                }
                setData(responseData["board"])
                let board = responseData["board"]
                const newGridData = JSON.parse(JSON.stringify(gridData));
                let x = responseData["x"] - 1;
                let y = 5 - responseData["y"];
                // modifies board with AI's move
                for (let i = 0; i < 6; i++) {
                    for (let j = 0; j < 7; j++) {
                        if (i === y && x === j) {
                            // tracks the most recent move by the AI
                            newGridData[y][x] = { ...newGridData[y][x], recent: true, AIPlace: true };
                        } else if (board[i][j] >= 1) {
                            newGridData[i][j] = { ...newGridData[i][j], isClicked: true };
                        } else if (board[i][j] <= -1) {
                            newGridData[i][j] = { ...newGridData[i][j], AIPlace: true, recent: false };
                        } else {
                            newGridData[i][j] = { ...newGridData[i][j], AIPlace: false, isClicked: false, recent: false };
                        }
                    }
                }
                setGridData(newGridData);
                setIsPlayerTurn(true);

            })
            .catch(error => {
                console.error("Error sending data:", error);
            });
    };
    // resets the board 
    function resetGameState() {
        setHasDraw(false)
        setHasLost(false)
        setHasWon(false)
        setIsGameActive(true)
        setInvalid(false)
        const newGridData = gridData.map(rowData => rowData.map(cell => {
            return { ...cell, isClicked: false, AIPlace: false, recent: false }; // Return a new object for other cells
        }));

        setGridData(newGridData);
    }

    function updateRecord(wins, losses, ties) {
        const recordRef = ref(database, 'Connect4/Record');
        set(recordRef, {
          Wins: wins,
          Losses: losses,
          Ties: ties
        }).then(() => {
          console.log('Record updated successfully');
        }).catch((error) => {
          console.error('Error updating record: ', error);
        });
      }
    return (
        <div className='game'>
            <div className="popup-container">
                <div className="popup-content">
                    <button className="close-button" onClick={onClose}>&times;</button>
                    <div className='board'>
                        <Grid gridData={gridData} onButtonClick={handleButtonClick} />
                    </div>
                </div>

                {hasWon &&
                    <div className="gameEnd-popup">
                        <div>
                            <h1>You Win!</h1>
                        </div>
                    </div>}
                {hasDraw &&
                    <div className="gameEnd-popup">
                        <div>
                            <h1>Draw!</h1>
                        </div>
                    </div>}
                {hasLost &&
                    <div className="gameEnd-popup">
                        <div>
                            <h1>You Lose!</h1>
                        </div>
                    </div>}
                {isInvalid &&
                    <div className="invalid-popup">
                        <div>
                            <h1>Invalid Move!</h1>
                        </div>
                    </div>}
                {(hasLost || hasDraw || hasWon) &&
                    <button id="playAgainButton" className="play-again-btn" onClick={resetGameState}>
                        Play Again?</button>
                }
                {isPlayerTurn && isGameActive && !isInvalid &&
                    <div className="move-popup">
                        <div>
                            <h1>Your turn</h1>
                        </div>
                    </div>}
                {!isPlayerTurn && isGameActive &&
                    <div className="move-popup">
                        <div>
                            <h1>AI's turn</h1>
                            <div className="lds-spinner"><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div></div>
                        </div>
                    </div>}
            </div>
        </div>
    );
}

export default Game;