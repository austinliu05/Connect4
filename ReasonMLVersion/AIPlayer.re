open! CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

// minimax: PlayerGame.state * int => float

/*
input: A PlayerGame.state s and int depth
output: A float value representing whether the state is more favorable for P1 
or P2 when looking down depth num moves

RECURSION DIAGRAM:

OI: (State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [-1, -1, -1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 2)
RI: (State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [-1, -1, -1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 1)
RO: 50.
Ideation Space: Because player 1 will win in the next state, return RO
OO: 50.

OI: State(
    Win(P1),
    [
      [1, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [-1, -1, -1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 2)
RI: N/A
RO: N/A
Ideation Space: Because inputted gamestatus is Win(P1), return 50.
OO: 50.
*/

  let rec minimax: (PlayerGame.state, int) => float =
    (s, depth) => {
    let moves = PlayerGame.legalMoves(s);
    let stateList = List.map(move => PlayerGame.nextState(s, move), moves);
      switch (depth) {
      | 0 => PlayerGame.estimateValue(s)
      | _ =>
        switch (PlayerGame.gameStatus(s)) {
        | Win(x) =>
          if (x == P1) {
            50.;
          } else {
            (-50.);
          };
        | Draw => 0.0
        | Ongoing(x) =>
          if (x == P1) {
            List.fold_left(
              max,
              -50.,
              List.map(state => minimax(state, depth - 1), stateList),
            );
          } else {
            List.fold_left(
              min,
              50.,
              List.map(state => minimax(state, depth - 1), stateList),
            );
          }
        }
      };
    };

// bestMove: PlayerGame.state * float * list(PlayerGame.move) => PlayerGame.move

/*
input: a PlayerGame.state s, a float evaluation, and a list of PlayerGame.move 
moveList
output: a PlayerGame.move that is evaluated by minimax to be the best move

RECURSION DIAGRAM:

OI: (State(
    Ongoing(P1),
    [
      [1, -1, -1, 1, 0],
      [-1, -1, 1, 0, 0],
      [-1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 50., [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6), Move(7)])
RI: (State(
    Ongoing(P1),
    [
      [1, -1, -1, 1, 0],
      [-1, -1, 1, 0, 0],
      [-1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 50., [Move(2), Move(3), Move(4), Move(5), Move(6), Move(7)])
RO: Move(4)
Ideation Space: Because move 4 results in win for P1 and 1 does not, return RO
OO: Move(4)

OI: (State(
    Win(P1),
    [
      [1, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [-1, -1, -1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 50., [])
RI: N/A
RO: N/A
Ideation Space: because moveList is empty, return failwith("cannot find 
best move")
OO: failwith("cannot find best move")
*/

  let rec bestMove:
    (PlayerGame.state, list(PlayerGame.move)) => (PlayerGame.move, float) =
    (s, moveList) => {
      switch (moveList) {
      | [] => failwith("cannot find best move")
      | [hd, ...tl] =>
        let eval = minimax(PlayerGame.nextState(s, hd), 3)
        if (eval >= snd(bestMove(s, tl))) {
          (hd, eval)
        } else {
          bestMove(s, tl);
        };
      };
    };

// nextMove: PlayerGame.state => PlayerGame.move

/*
input: a PlayerGame.state s
output: a PlayerGame.move that plays the move evaluated by minimax to be the 
best move
*/

  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let moves = PlayerGame.legalMoves(s);
      fst(bestMove(s, moves))
    };

  let playerName = "Max and Austin";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer;

/* insert test cases for any procedures that don't take in
 * or return a state here */

checkExpect(minimax(State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [1, 1, -1, 1, 0],
      [-1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 2), 50., "test minimax favoring P1");

checkExpect(minimax(State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 2), -50., "test minimax favoring P2");

checkExpect(minimax(State(
    Win(P1),
    [
      [1, 0, 0, 0, 0],
      [-1, 1, 0, 0, 0],
      [-1, -1, 1, 0, 0],
      [1, -1, -1, 1, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ), 2), 50., "test basecase minimax - Win(P1)");

checkExpect(minimax(State(
    Draw,
    [
      [1, -1, 1, -1, 1],
      [-1, 1, -1, 1, -1],
      [-1, 1, -1, 1, -1],
      [-1, 1, -1, 1, -1],
      [1, -1, 1, -1, 1],
      [1, -1, 1, -1, 1],
      [1, -1, 1, -1, 1],
    ],
  ), 2), 0.0, "test basecase minimax - Draw");

// checkExpect(bestMove(State(
//     Ongoing(P1),
//     [
//       [1, -1, -1, 1, 0],
//       [-1, -1, 1, 0, 0],
//       [-1, 1, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//     ],
//   ), [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6), Move(7)])
//   ,Move(4), "test bestMove - P1");

// checkExpect(bestMove(State(
//     Ongoing(P2),
//     [
//       [0, 0, 0, 0, 0],
//       [1, -1, 0, 0, 0],
//       [1, 1, -1, 0, 0],
//       [-1, 1, 1, -1, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//     ],
//   ), -50., [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6), Move(7)]), 
//   Move(1), "test bestMove - P2");

// checkError(() => bestMove(State(
//     Win(P1),
//     [
//       [1, 0, 0, 0, 0],
//       [-1, 1, 0, 0, 0],
//       [-1, -1, 1, 0, 0],
//       [1, -1, -1, 1, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//       [0, 0, 0, 0, 0],
//     ],
//   ), 50., []), "cannot find best move");