# net-chess

## Milestone 1: Proposal
In this project, we plan to implement a 2-player chess game over network. The board position is shown as TUI using brick library which keeps updating after each player move. The game needs to be played using the [chess moves notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)). If a player enters an invalid move, We ask the player to provide a different move. There are various cases that needs to be handled while determining if a move is legal or not. 
We will use network library for handling p2p communication. 
If time permits, we also plan to create a player vs computer chess game using a pre-built haskell chess engine.

### Basic requirements
1. A TUI that shows chess board (or some representation that resembles chess board.) over network for both the players.
2. A player should be able to enter chess moves and the board should be updated after every move. (Here we are assuming that the player gives only valid moves.)
3. Each player will have timer that shows their available time.

### Extra requirements
1. Ability to determine if a move is legal or not. 
2. Ability to play against computer.

### Collaborators 
1. Name : Sri Koripalli, e-mail: skoripalli@ucsd.edu
2. Name : Bavanya K, e-mail: bkurra@ucsd.edu

## Milestone 2: Updates

### What is the architecture of your application (the key components)?
- Our MVP which is to build a two player game in a single instance has the following components: 
    - The primary game logic resides in the Chess.hs file: 
        - The logic in Chess.hs file will check if a move is valid:
          </br>The piece at from_position is of required color. Also, there is no piece at to_position or piece at to_position is of opposite color.
        - Generate updated state of the board after each move.
    - User Prompt: 
        - from_position and to_position will be specified by the user when it's their turn to make a move.
    - TUI visualization: 
        - Logic responsible for configuring brick application, handling game events like rendering board in the terminal and handling user prompts resides in UI.hs.
- We are yet to design the architecture of the networked application.

### What challenges (if any) did you have so far and how did you solve them?
- We are yet to implement keeping track of who's turn (white or black) it is to make the move and we will take help from the state transformers concepts taught in class.
- Since we are new to using the brick library, we are still trying to figure out how to build a TUI visualization of the chess board state using brick. We are planning to take reference from the sudoku implementation example on brick's github repo to understand how to build a 8x8 square.
- We need to find a way to visualize the chess pieces in the TUI. We are yet to verify if our idea of using unicode characters work with the brick library. A simple fall-back option is to use small and capital letters to represent the chess pieces, for eg: p represents white Pawn and P represents black Pawn.

### Do you expect to meet your goals until the deadline?
- We are not sure if we will have time to implement the networked part of the two-player application. 

### If not, how will you modify your goals?
- If we do not have enough time to implement the networked part, 
we will alternatively change the turn of the user in the TUI by enforcing the following constraint:
    - Only a white piece can be moved when it is white team's turn and vice versa.




