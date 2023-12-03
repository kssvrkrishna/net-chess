module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))
import qualified Data.Text as T

data CounterState = CounterState { count :: Int, count1 :: Int } deriving (Read, Show)
data CounterState1 = CounterState1 { count_dec :: Int, count_dec1 :: Int } deriving (Read, Show)

data Color = Black | White deriving (Eq, Show, Read)
type Position = (Char, Int)

data Square = Square {position :: Position, piece :: Maybe Piece } deriving (Show, Read)
type Chessboard = [Square]

data PieceType = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq, Show, Read)
data Piece = Piece PieceType Color deriving (Eq, Show, Read)

getColor :: Piece -> Color
getColor (Piece _ color) = color

-- Defined this way to comply with the algebraic notation of chess moves.
data Move = Move {movePiece :: Piece, toPosition :: Position, isCapture :: Bool} deriving (Eq, Show) 

data Game = Game
  { cursor :: (Int, Int)
  , previous :: Maybe Game
  , counterState :: CounterState
  , counterState1 :: CounterState1
  , currentPlayerTurn :: Color
  , board :: Chessboard
  , inputChars :: T.Text
  } deriving (Read, Show)

increment :: Game -> Game
increment game =
  let updatedCounterState = (counterState game) { count = count (counterState game) + 1
                                               , count1 = count1 (counterState game) + 1
                                               }
      updatedGame = game { counterState = updatedCounterState }
  in togglePlayerTurn (updatedGame { inputChars = T.empty })

decrement :: Game -> Game
decrement game =
  let updatedCounterState = (counterState game) { count = count (counterState game) - 1
                                               , count1 = count1 (counterState game) - 1
                                               }
      updatedGame = game { counterState = updatedCounterState }
  in togglePlayerTurn (updatedGame { inputChars = T.empty })

opponent :: Color -> Color
opponent White = Black
opponent Black = White

togglePlayerTurn :: Game -> Game
togglePlayerTurn game = game { currentPlayerTurn = opponent (currentPlayerTurn game) }

initialChessBoard :: Chessboard
initialChessBoard =
  [ Square ('a', 1) (Just (Piece Rook   White))
  , Square ('b', 1) (Just (Piece Knight White))
  , Square ('c', 1) (Just (Piece Bishop White))
  , Square ('d', 1) (Just (Piece Queen  White))
  , Square ('e', 1) (Just (Piece King   White))
  , Square ('f', 1) (Just (Piece Bishop White))
  , Square ('g', 1) (Just (Piece Knight White))
  , Square ('h', 1) (Just (Piece Rook   White))
  ]
  ++
  [ Square (col, 2) (Just (Piece Pawn White)) | col <- ['a'..'h'] ]
  ++
  [ Square (col, 7) (Just (Piece Pawn Black)) | col <- ['a'..'h'] ]
  ++
  [ Square ('a', 8) (Just (Piece Rook   Black))
  , Square ('b', 8) (Just (Piece Knight Black))
  , Square ('c', 8) (Just (Piece Bishop Black))
  , Square ('d', 8) (Just (Piece Queen  Black))
  , Square ('e', 8) (Just (Piece King   Black))
  , Square ('f', 8) (Just (Piece Bishop Black))
  , Square ('g', 8) (Just (Piece Knight Black))
  , Square ('h', 8) (Just (Piece Rook   Black))
  ]
  ++
  [ Square (col, row) Nothing | col <- ['a'..'h'], row <- [3..6] ]
