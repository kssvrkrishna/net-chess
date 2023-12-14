module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))
import qualified Data.Text as T
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Internal.Read (digitToInt)
import Data.List (find)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

debugLog :: String -> IO ()
debugLog msg = withFile "debug.log" AppendMode (\handle -> hPutStrLn handle msg)


data CounterState = CounterState { count :: Int, count1 :: Int } deriving (Read, Show)
data CounterState1 = CounterState1 { count_dec :: Int, count_dec1 :: Int } deriving (Read, Show)

data Color = Black | White deriving (Eq, Show, Read)
type Position = (Char, Int)

data Square = Square {position :: Position, piece :: Maybe Piece } deriving (Show, Read)
type Chessboard = [Square]

-- K for king, Q for queen, R for rook, B for bishop, and N for knight
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
  in togglePlayerTurn (updatedGame {previous = Just game, inputChars = T.empty })

decrement :: Game -> Game
decrement game =
  let updatedCounterState = (counterState game) { count = count (counterState game) - 1
                                               , count1 = count1 (counterState game) - 1
                                               }
      updatedGame = game { counterState = updatedCounterState }
  in togglePlayerTurn (updatedGame {previous = Just game, inputChars = T.empty })

move :: Game -> Game
move game = game

captureMove :: Game -> Game
captureMove game = game

-- pawnMove :: Game -> Game
-- pawnMove game = game

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
  [ Square (col, row) Nothing | row <- [3..6],col <- ['a'..'h']]
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

textToPosition :: T.Text -> Position
textToPosition text = 
  let col = T.head text
      row = T.unpack text !! 1
  in (col, read [row])

updateBoard :: Chessboard -> Position -> Position -> Chessboard
updateBoard board from to =
  let movePiece square
        | position square == from = Square from Nothing
        | position square == to = Square to (findPiece board from)
        | otherwise = square
  in map movePiece board

findPiece :: Chessboard -> Position -> Maybe Piece
findPiece board pos = piece (head (filter (\s -> position s == pos) board))

pawnMove :: Game -> T.Text -> Game
pawnMove game moveText =
  let fromPos = case currentPlayerTurn game of
                  White -> textToPosition (T.snoc (T.take 1 moveText) '2')
                  Black -> textToPosition (T.snoc (T.take 1 moveText) '7')
      toPos = textToPosition moveText
      newBoard = updateBoard (board game) fromPos toPos
  in game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }


handleThreeLengthMove :: Game -> T.Text -> Game
handleThreeLengthMove game moveText
  | T.length moveText == 3 = case T.unpack moveText of
                               [p, col, row] -> handlePieceMove game p (col, digitToInt row)
                               _ -> error "Invalid move format"
  | otherwise = error "Invalid move length"
  where
    handlePieceMove :: Game -> Char -> Position -> Game
    handlePieceMove g p (c, r) =
      let pieceType = charToPieceType p
          fromPos = findPiecePosition (board g) pieceType (currentPlayerTurn g)
          toPos = (c, r)
          newBoard = updateBoard (board g) fromPos toPos
      in g { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn g) }

    charToPieceType :: Char -> PieceType
    charToPieceType c = case toLower c of
                          'n' -> Knight
                          'b' -> Bishop
                          'r' -> Rook
                          'q' -> Queen
                          'k' -> King
                          _   -> error "Invalid piece type"

-- Function to find the position of a piece on the board
findPiecePosition :: Chessboard -> PieceType -> Color -> Position
findPiecePosition board pt color = 
    case find (\(Square _ mp) -> 
                case mp of
                    Just (Piece pt' color') -> pt == pt' && color == color'
                    Nothing -> False) board of
        Just (Square pos _) -> pos
        Nothing -> error "Piece not found"