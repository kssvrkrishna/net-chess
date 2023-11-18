# net-chess

## Proposal
In this project, we plan to implement a 2-player chess game over network. The board position is shown as TUI using brick library which keeps updating after each player move. The game needs to be played using the [chess moves notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)). If a player enters an invalid move, We ask the player to provide a different move. There are various cases that needs to be handled while determining if a move is legal or not. 
We will use network library for handling p2p communication. 
If time permits, we also plan to create a player vs computer chess game using a pre-built haskell chess engine.

### Basic requirements
1. A TUI that shows chess board (or some representation that resembles chess board.) over network for both the players.
2. A player should be able to enter chess moves and the board should be updated after every move. (Here we are assuming that the player gives only valid moves.)
3. Each player will have timer that shows his available time.

### Extra requirements
1. Ability to determine if a move is legal or not. 
2. Ability to play against computer.

### Collaborators 
1. Name : Sri Koripalli, e-mail: skoripalli@ucsd.edu
2. Name : Bavanya K, e-mail: bkurra@ucsd.edu






