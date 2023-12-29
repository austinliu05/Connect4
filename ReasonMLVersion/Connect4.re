open CS17SetupGame;
open Game;

module Connect4 = {
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  type state =
    | State(status, list(list(int)));
  type move =
    | Move(int);

  // initialState: string => state

  /*
   input: an initializing string s representing the dimensions of the game board
   output: a state which includes a board with the dimensions represented
   */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      /* your initial state, using boardHeight and boardWidth, goes here */
      // creates a blank row
      let rec createRow: int => list(int) =
        w => {
          switch (w) {
          | 0 => []
          | _ => [0, ...createRow(w - 1)]
          };
        };
      // stacks rows repeatedly to create board
      let rec createBoard: (list(int), int) => list(list(int)) =
        (lst, h) => {
          switch (h) {
          | 0 => []
          | _ => [lst, ...createBoard(lst, h - 1)]
          };
        };
      State(Ongoing(P1), createBoard(createRow(boardHeight), boardWidth));
    };

  // legalMoves: state => list(move)

  /*
   input: state s that represents the state of the board
   output: a list(move) that is a list of all the legal moves at s
   */

  /* produce the set of legal moves at a state, represented as a list */
  let legalMoves: state => list(move) =
    s =>
      switch (s) {
      | State(Win(_), _)
      | State(Draw, _) => []
      | State(Ongoing(_), mat) =>
        // checks if current row is full
        let rec checkRow: list(int) => bool = (
          lst =>
            switch (lst) {
            | [] => false
            | [0, ..._] => true
            | [_, ...tl] => checkRow(tl)
            }
        );
        // returns rows where there is space to insert game piece (1 based index)
        let rec checkMatrix: (list(list(int)), int) => list(move) = (
          (mat, n) =>
            switch (mat) {
            | []
            | [[]] => []
            | [hd, ...tl] =>
              if (checkRow(hd)) {
                [Move(n), ...checkMatrix(tl, n + 1)];
              } else {
                checkMatrix(tl, n + 1);
              }
            }
        );
        checkMatrix(mat, 1);
      };

  // gameStatus: state => status

  /*
   input: state s that represents the state of the board
   output: the status of the game at the given state
   */

  /* returns the status of the game at the given state */
  let gameStatus: state => status =
    s => {
      switch (s) {
      | State(a, _) => a
      };
    };

  // transpose: list(list(int)) => list(list(int))

  /*
   Input: a list(list(int)) mat which may be square or rectangular
   Output: a list(list(int)) that returns mat transposed. The transpose of a
   list(list(int)) is another list(list(int)) that results from reflecting the
   original list(list(int)) across its main diagonal.

   RECURSION DIAGRAMS
   Original input: [[1, 7], [10, 11]]
   Recursive input: [[7], [11]]
   Recursive output: [[7, 11]]
   Ideation space: Take removed column and
   add as a row to the recursive output
   Original Output: [[1, 10], [7, 11]]

   Original intput: []
   Recursive input: N/A
   Recursive output: N/A
   Ideation space: If input is [],
   output "A matrix cannot be 0 - dimensional ."
   Original output: "A matrix cannot be 0 - dimensional ."
   */

  let rec transpose: list(list(int)) => list(list(int)) =
    mat =>
      switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0 - dimensional.")
      | [[_], ..._] => [List.flatten(mat)]
      | [[_, ..._], ..._] => [
          List.map(List.hd, mat),
          ...transpose(List.map(List.tl, mat)),
        ]
      };

  // nextState: state * move => state

  /*
   input: a state s and a legal move m
   output: the state of the game after the move is made
   */

  /* given a state and a legal move, yields the next state */
  let nextState: (state, move) => state =
    (s, m) =>
      switch (s, m) {
      | (State(Ongoing(x), mat), Move(y)) =>
        // checks what type of player is playing
        let getdiscType: whichPlayer => int = (
          p => {
            switch (p) {
            | P1 => 1
            | P2 => (-1)
            };
          }
        );
        let disc = getdiscType(x);
        // iterates through list, finds row that corresponds to move and inserts
        let rec makeMove: (list(list(int)), int, int) => list(list(int)) = (
          (mat, move, value) => {
            switch (mat) {
            | [[]]
            | [] => []
            | [hd, ...tl] =>
              // inserts in the first open spot of row
              let rec insert: list(int) => list(int) = (
                lst => {
                  switch (lst) {
                  | [] => []
                  | [0, ...tl] => [value, ...tl]
                  | [x, ...tl] => [x, ...insert(tl)]
                  };
                }
              );
              // if at the correspoding row, insert and return row
              if (move - 1 == 0) {
                [insert(hd), ...tl];
              } else {
                [
                  //else, recursively call
                  hd,
                  ...makeMove(tl, move - 1, value),
                ];
              };
            };
          }
        );
        // checks for four discs in a row (4 for P1, and -4 for P2)
        let rec checkFour: (list(int), int, int) => bool = (
          (lst, discType, count) =>
            if (count == 4 || count == (-4)) {
              true;
            } else {
              switch (lst) {
              | [] => false
              | [hd, ...tl] =>
                if (hd == discType) {
                  checkFour(tl, discType, discType + count);
                } else {
                  checkFour(tl, discType, 0);
                }
              };
            }
        );
        // calls checkFour on every row in the board
        let rec checkBoard: list(list(int)) => bool = (
          mat => {
            switch (mat) {
            | []
            | [[]] => false
            | [hd, ...tl] =>
              if (checkFour(hd, disc, 0)) {
                true;
              } else {
                checkBoard(tl);
              }
            };
          }
        );
        // checks for four in a row diagonally
        let rec checkDiagonal: (list(list(int)), int, int) => bool = {
          (
            (mat, discType, count) =>
              // ending base case
              if (count == 4) {
                true;
              } else {
                switch (mat) {
                | []
                | [[], ..._] => false
                | [[hd, ...tl], ...tl2] =>
                  let rec tails: list(list(int)) => list(list(int)) = (
                    mat => {
                      switch (mat) {
                      | []
                      | [[]] => []
                      | [[], ...tl] => tails(tl)
                      | [[_, ...tl], ...tl2] => [tl, ...tails(tl2)]
                      };
                    }
                  );
                  if (hd == discType) {
                    /*  takes the hd of every row in the current matrix. After
                        checking to see if the hd is the correct discType or not,
                        we call List.tl on every row after to essentially move to
                        the right along the diagonal.

                        [[1,0,0,0],             [[1,0,0,0],    We only check the hd
                        [0,1,0,0],  turns into  [1,0,0],      for every row. In this
                        [0,0,1,0],              [1,0],        case, there are 4 1,
                        [0,0,0,1]]              [1]]          therefore, count == 4
                                                              and func returns true
                        */
                    checkDiagonal(tails(tl2), discType, count + 1)
                    /*  checks the diagonal to the right
                        [[1,0,0,0],             [[0,0,0],
                         [0,1,0,0],  turns into  [1,0,0]
                         [0,0,1,0],              [0,1,0],
                         [0,0,0,1]]              [0,0,1]]     */
                    || checkDiagonal([tl, ...tails(tl2)], discType, 0)
                    /*  checks the diagonal below
                        [[1,0,0,0],
                         [0,1,0,0],  turns into  [0,1,0,0]
                         [0,0,1,0],              [0,0,1,0],
                         [0,0,0,1]]              [0,0,0,1]]     */
                    || checkDiagonal(tl2, discType, 0);
                  } else {
                    // same thing as above but count does not change because the
                    // the current disc is not the correct type
                    checkDiagonal(tails(tl2), discType, 0)
                    || checkDiagonal([tl, ...tails(tl2)], discType, 0)
                    || checkDiagonal(tl2, discType, 0);
                  };
                };
              }
          );
        };
        let board = makeMove(mat, y, disc);
        if(List.length(legalMoves(State(Ongoing(x),board))) == 0){
          State(Draw, board)
        }else{
          // check column
        if (checkFour(List.nth(board, y - 1), disc, 0)) {
          State
            (Win(x), board);
            // check row
        } else if (checkBoard(transpose(board))) {
          State
            (Win(x), board);
            // check diagonal
        } else if (checkDiagonal(board, disc, 0)
                   || checkDiagonal(List.rev(board), disc, 0)) {
          State(Win(x), board);
        } else if (x == P1) {
          State(Ongoing(P2), board);
        } else {
          State(Ongoing(P1), board);
        };
        };
      | (a, _) => a
      };

  // currentPlayer: state => whichPlayer

  /*
   input: a state s
   output: the current player of the game (whichPlayer) in state s
   */

  /* returns the current player of the game */
  let currentPlayer: state => whichPlayer =
    fun
    | State(Ongoing(x), _) => x
    | State(Win(x), _) => x
    | _ => failwith("neither play wins, its a draw");

  // estimateValue: state => float

  /*
   input: a state s
   output: an estimate of the value (float), to player 1, of state s. The value
   must be positive when player 1 is winning, negative for when player 1 is losing,
   and intermediate values when winning/losing is determined by the number of
   discs in the same row.
   */

  /* estimate the value of a given state */
  let estimateValue: state => float =
    inState =>
      switch (inState) {
      | State(Ongoing(_), mat) =>
        // checks for horwever many discs in a row
        let rec rowValue: (list(int), int, int) => int = (
          (lst, score, disc) => {
            switch (lst) {
            | [] => score
            // 3 in a row
            | [hd1, hd2, hd3, hd4, ...tl] =>
              if (hd1 == hd2 && hd2 == hd3 && hd3 == disc) {
                // if 3 in a row has an open end
                if (hd4 == 0) {
                  rowValue(tl, 15 * disc + score, disc);
                } else {
                  // if 3 in a row is blocked off
                  rowValue(
                    tl,
                    3 * disc + score,
                    disc,
                  );
                };
              } else {
                rowValue([hd2, hd3, hd4, ...tl], score, disc);
              }
            // 2 in a row
            | [hd1, hd2, hd3, ...tl] =>
              if (hd1 == hd2 && hd2 == disc) {
                // if 2 in a row has open end
                if (hd3 == 0) {
                  rowValue(tl, 5 * disc + score, disc);
                } else {
                  // if 2 in a row is blocked off
                  rowValue(
                    tl,
                    3 * disc + score,
                    disc,
                  );
                };
              } else {
                rowValue([hd2, hd3, ...tl], score, disc);
              }
            // 1 in a row
            | [hd1, hd2, ...tl] =>
              if (hd1 == disc) {
                // if 1 in a row is open
                if (hd2 == 0) {
                  rowValue(tl, disc + score, disc);
                } else {
                  // if 1 in a row is blocked off
                  rowValue(tl, score, disc);
                };
              } else {
                rowValue([hd2, ...tl], score, disc);
              }
            | [hd, ...tl] => rowValue(tl, score + hd, disc)
            };
          }
        );
        // similar helper to check diagonal above but it counts the number
        // of discs in a row instead
        let rec checkDiagonal: (list(list(int)), int, int) => int = {
          (
            (mat, discType, count) => {
              switch (mat) {
              | []
              | [[], ..._] => count
              | [[hd, ...tl], ...tl2] =>
                let rec tails: list(list(int)) => list(list(int)) = (
                  mat => {
                    switch (mat) {
                    | []
                    | [[]] => []
                    | [[], ...tl] => tails(tl)
                    | [[_, ...tl], ...tl2] => [tl, ...tails(tl2)]
                    };
                  }
                );
                if (hd == 1) {
                  if (count == 0) {
                    max(
                      max(
                        checkDiagonal(
                          tails(tl2),
                          discType,
                          count + discType,
                        ),
                        checkDiagonal(
                          [tl, ...tails(tl2)],
                          discType,
                          count,
                        ),
                      ),
                      checkDiagonal(tl2, discType, count),
                    );
                  } else {
                    max(
                      max(
                        checkDiagonal(
                          tails(tl2),
                          discType,
                          count + 5 * discType,
                        ),
                        checkDiagonal(
                          [tl, ...tails(tl2)],
                          discType,
                          count,
                        ),
                      ),
                      checkDiagonal(tl2, discType, count),
                    );
                  };
                }else if (hd == -1) {
                 if (count == 0) {
                    min(
                      min(
                        checkDiagonal(
                          tails(tl2),
                          discType,
                          count + discType,
                        ),
                        checkDiagonal(
                          [tl, ...tails(tl2)],
                          discType,
                          count,
                        ),
                      ),
                      checkDiagonal(tl2, discType, count),
                    );
                  } else {
                    min(
                      min(
                        checkDiagonal(
                          tails(tl2),
                          discType,
                          count + 5 * discType,
                        ),
                        checkDiagonal(
                          [tl, ...tails(tl2)],
                          discType,
                          count,
                        ),
                      ),
                      checkDiagonal(tl2, discType, count),
                    );
                  };
                }else {
                  max(
                    max(
                      checkDiagonal(tails(tl2), discType, count),
                      checkDiagonal([tl, ...tails(tl2)], discType, count),
                    ),
                    checkDiagonal(tl2, discType, count),
                  );
                };
              };
            }
          );
        };
        let rec iterate: (list(list(int)), int, int) => int = (
          (mat, score, disc) => {
            switch (mat) {
            | [] => score
            | [hd, ...tl] =>
              iterate(tl, score + rowValue(hd, 0, disc), disc)
            };
          }
        );
        let row1 = iterate(mat, 0, 1);
        let row2 = iterate(mat, 0, -1);
        let col1 = iterate(transpose(mat), 0, 1);
        let col2 = iterate(transpose(mat), 0, -1);
        let diag1 = checkDiagonal(mat, 0, 1);
        let diag2 = checkDiagonal(mat, 0, -1);
        let diag3 = checkDiagonal(List.rev(mat), 0, 1);
        let diag4 = checkDiagonal(List.rev(mat), 0, -1);
        float_of_int(max(max(max(row1, col1), diag1),diag3))
        +. float_of_int(min(min(min(row2, col2), diag2),diag4));
      | State(Win(P1), _) => 50.
      | State(Win(P2), _) => (-50.)
      | State(Draw, _) => 0.
      };

  // stringOfPlayer: whichPlayer => string

  /*
   input: a whichPlayer
   output: the string version of whichPlayer
   */

  let stringOfPlayer: whichPlayer => string =
    fun
    | P1 => "P1"
    | P2 => "P2";

  // stringOfState: state => string

  /*
   input: a state
   output: a string representation of the game state
   */

  let stringOfState: state => string =
    fun
    | State(Ongoing(_), y) => {
        let rec printBoard: list(list(int)) => string = (
          lst => {
            switch (lst) {
            | [] => ""
            | [[]] => ""
            | [[], ...tl] => "\n\n" ++ printBoard(tl)
            | [[hd, ...tl], ...tl2] =>
              if (hd == 1) {
                " | + | " ++ printBoard([tl, ...tl2]);
              } else if (hd == 0) {
                " | O | " ++ printBoard([tl, ...tl2]);
              } else {
                " | - | " ++ printBoard([tl, ...tl2]);
              }
            };
          }
        );
        "Board: \n" ++ printBoard(List.rev(transpose(y)));
      }
    | State(_, _) => "Game Over";

  // stringOfMove: move => string

  /*
   input: a move
   output: a string representation of the move
   */

  let stringOfMove: move => string =
    fun
    | Move(x) => string_of_int(x);

  // moveOfString: string * state => move

  /*
   input: a string representation of a move str and a state st
   output: an internal representation of a move, given str and st
   */

  let moveOfString: (string, state) => move =
    (str, st) => {
      let isInteger: string => int =
        s =>
          try(int_of_string(s)) {
          | _ => failwith("input has to be an integer")
          };
      let move = isInteger(str);
      if (List.mem(Move(move), legalMoves(st))) {
        Move(move);
      } else {
        failwith("invalid move");
      };
    };
};

module MyGame: Game = Connect4;
open Connect4;

/* test cases */
// checks for legal moves
checkExpect(
  legalMoves(initialState("5 7")),
  [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6), Move(7)],
  "Successfully found all legal moves!",
);
checkExpect(
  nextState(initialState("1 3"), Move(1)),
  State(Ongoing(P2), [[1], [0], [0]]),
  "Successfully made a move!",
);
print_endline(stringOfState(initialState("5 7")));

// checkin if nextState is correct
let board1 =
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  );
checkExpect(
  nextState(board1, Move(4)),
  State(
    Ongoing(P2),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "Success! Player 1 made a move in column 4!",
);

let board3 =
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [(-1), 1, 1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  );
checkExpect(
  nextState(board3, Move(4)),
  State(
    Win(P1),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [(-1), 1, 1, 1, 1],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "Success! Player 1 won!",
);
let board4 =
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [(-1), 1, 0, 0, 0],
      [(-1), (-1), 1, 0, 0],
      [(-1), (-1), (-1), 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  );
checkExpect(
  nextState(board4, Move(1)),
  State(
    Win(P1),
    [
      [1, 0, 0, 0, 0],
      [(-1), 1, 0, 0, 0],
      [(-1), (-1), 1, 0, 0],
      [(-1), (-1), (-1), 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "Success! Player 1 won!",
);
checkExpect(
  nextState(board4, Move(5)),
  State(
    Ongoing(P2),
    [
      [0, 0, 0, 0, 0],
      [(-1), 1, 0, 0, 0],
      [(-1), (-1), 1, 0, 0],
      [(-1), (-1), (-1), 1, 0],
      [1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "Success! Player 1 made a move in column 5!",
);
// checks if transpose works
checkExpect(
  transpose([[1], [2], [3]]),
  [[1, 2, 3]],
  "Transposed successfully on a 3x1 column (base case)",
);
checkExpect(
  transpose([[1, 7], [10, 11]]),
  [[1, 10], [7, 11]],
  "Transposed successfully on a 2 x 3 matrix",
);
checkExpect(
  transpose([[1, 2, 3]]),
  [[1], [2], [3]],
  "Transposed successfully on a 1x3 row",
);
