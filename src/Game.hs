module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))
import Data.List (sortBy)

import qualified Data.Text as T
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Internal.Read (digitToInt)
import Data.Maybe (listToMaybe)

import Data.List (find)
import Data.Char (chr, ord)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

debugLog :: String -> IO ()
debugLog msg = withFile "debug.log" AppendMode (\handle -> hPutStrLn handle msg)

data Color = Black | White deriving (Eq, Show, Read)
type Position = (Char, Int)

data Square = Square {position :: Position, piece :: Maybe Piece } deriving (Eq, Show, Read)
type Chessboard = [Square]

data PieceType = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq, Show, Read)
data Piece = Piece PieceType Color deriving (Eq, Show, Read)

getColor :: Piece -> Color
getColor (Piece _ color) = color

data Move = Move {movePiece :: Piece, toPosition :: Position, isCapture :: Bool} deriving (Eq, Show) 

data Game = Game
  { cursor :: (Int, Int)
  , previous :: Maybe Game
  , currentPlayerTurn :: Color
  , board :: Chessboard
  , inputChars :: T.Text
  , moveHistory :: [T.Text]
  } deriving (Eq, Read, Show)

opponent :: Color -> Color
opponent White = Black
opponent Black = White

togglePlayerTurn :: Game -> Game
togglePlayerTurn game = game { currentPlayerTurn = opponent (currentPlayerTurn game) }

updateGameWithMove :: Game -> T.Text -> Game
updateGameWithMove game moveText =
    game { moveHistory =  (moveHistory game) ++ [moveText]  }

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
  let toPos@(col, row) = textToPosition moveText
      maybeFromPos = findPawnPosition (board game) (currentPlayerTurn game) col row
  in case maybeFromPos of
       Just fromPos -> 
           let newBoard = updateBoard (board game) fromPos toPos
           in game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }
       Nothing -> error "No pawn found for move"


findPawnPosition :: Chessboard -> Color -> Char -> Int -> Maybe Position
findPawnPosition board color col targetRow =
    let pawns = filter isPawnOnColumn board
        isPawnOnColumn (Square (c, r) (Just (Piece Pawn colr))) = c == col && colr == color && isValidPawnMove r targetRow color
        isPawnOnColumn _ = False
        validPawns = filter (\(Square pos _) -> isValidPawnMove (snd pos) targetRow color) pawns
    in fmap position $ listToMaybe validPawns

isValidPawnMove :: Int -> Int -> Color -> Bool
isValidPawnMove currentRow targetRow color =
    let rowDiff = targetRow - currentRow
    in if color == White
       then (rowDiff == 1 || (currentRow == 2 && rowDiff == 2))
       else (rowDiff == -1 || (currentRow == 7 && rowDiff == -2))

-- TODO: Write test!
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
          targetPos = (c, r)
          maybeFromPos = findMovablePiece (board g) pieceType (currentPlayerTurn g) targetPos
      in case maybeFromPos of
           Just fromPos ->
             let newBoard = updateBoard (board g) fromPos targetPos
             in g { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn g) }
           Nothing -> error "No movable piece found"

charToPieceType :: Char -> PieceType
charToPieceType c = case toLower c of
                        'n' -> Knight
                        'b' -> Bishop
                        'r' -> Rook
                        'q' -> Queen
                        'k' -> King
                        _   -> error "Invalid piece type"

findMovablePiece :: Chessboard -> PieceType -> Color -> Position -> Maybe Position
findMovablePiece board pieceType color targetPos =
    let possiblePieces = filter isCorrectPiece board
        isCorrectPiece (Square pos (Just (Piece pt c))) = pt == pieceType && c == color && canMoveTo pos targetPos pt
        isCorrectPiece _ = False
    in case possiblePieces of
         (Square pos _):_ -> Just pos
         _                -> Nothing

canMoveTo :: Position -> Position -> PieceType -> Bool
canMoveTo from to Knight = to `elem` knightMoves from
canMoveTo from to Rook = to `elem` lineMoves from [(-1,0), (1,0), (0,-1), (0,1)]
canMoveTo from to Bishop = to `elem` lineMoves from [(-1,-1), (-1,1), (1,-1), (1,1)]
canMoveTo from to Queen = to `elem` lineMoves from [(-1,0), (1,0), (0,-1), (0,1), (-1,-1), (-1,1), (1,-1), (1,1)]
canMoveTo from to King = to `elem` adjacentMoves from
canMoveTo _ _ _ = False

lineMoves :: Position -> [(Int, Int)] -> [Position]
lineMoves (col, row) directions = concatMap (lineMove (col, row)) directions
  where
    lineMove :: Position -> (Int, Int) -> [Position]
    lineMove start@(c, r) (dc, dr) = takeWhile validPosition $ tail $ iterate (\(c, r) -> (chr (ord c + dc), r + dr)) start

adjacentMoves :: Position -> [Position]
adjacentMoves (col, row) = filter validPosition 
    [(chr (ord col + dc), row + dr) | dc <- [-1, 0, 1], dr <- [-1, 0, 1], (dc, dr) /= (0, 0)]


knightMoves :: Position -> [Position]
knightMoves (col, row) = filter validPosition 
    [(chr (ord col + dx), row + dy) | dx <- [-2, -1, 1, 2], dy <- [-2, -1, 1, 2], abs dx /= abs dy]


validPosition :: Position -> Bool
validPosition (col, row) = col `elem` ['a'..'h'] && row `elem` [1..8]

data MoveType = PawnCapture | PieceCapture | PawnPromotion | DisambiguatingMove deriving (Eq, Show)

determineMoveType :: T.Text -> MoveType
determineMoveType moveText
  | T.length moveText /= 4 = error "Invalid move length"
  | T.head moveText `elem` ['a'..'h'] && T.any (== 'x') moveText = PawnCapture
  | T.any (== 'x') moveText = PieceCapture
  | T.any (== '=') moveText = PawnPromotion
  | otherwise = DisambiguatingMove

-- TODO: Write test!
handleFourLengthMove :: Game -> T.Text -> Game
handleFourLengthMove game moveText = 
    case determineMoveType moveText of
        PawnCapture -> 
            let [colFrom, 'x', colTo, rowTo] = T.unpack moveText
            in handlePawnCapture game colFrom (colTo, digitToInt rowTo)

        PieceCapture -> 
            let [piece, 'x', col, row] = T.unpack moveText
            in handlePieceCapture game piece (col, digitToInt row)

        PawnPromotion -> 
            let [col, row, '=', newPiece] = T.unpack moveText
            in handlePawnPromotion game (col, digitToInt row) newPiece

        DisambiguatingMove -> 
            let [piece, disambiguator, col, row] = T.unpack moveText
            in handleDisambiguatingMove game piece disambiguator (col, digitToInt row)

-- TODO: Write test for invalid move!
handlePawnCapture :: Game -> Char -> Position -> Game
handlePawnCapture game colFrom (colTo, rowTo) =
    let fromPos = (colFrom, if currentPlayerTurn game == White then rowTo - 1 else rowTo + 1)
        toPos = (colTo, rowTo)
        newBoard = updateBoard (board game) fromPos toPos
    in game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }

-- TODO: Write test for invalid move!
handlePieceCapture :: Game -> Char -> Position -> Game
handlePieceCapture game piece (col, row) =
    let pieceType = charToPieceType piece
        maybeFromPos = findMovablePiece (board game) pieceType (currentPlayerTurn game) (col, row)
    in case maybeFromPos of
        Just fromPos -> 
            let newBoard = updateBoard (board game) fromPos (col, row)
            in game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }
        Nothing -> error "No piece found for capture"

-- TODO: Write test for invalid move!
handlePawnPromotion :: Game -> Position -> Char -> Game
handlePawnPromotion game (col, row) newPiece =
    let promotionRow = if currentPlayerTurn game == White then 8 else 1
        fromPos = (col, if currentPlayerTurn game == White then 7 else 2)
        toPos = (col, row)
        promotedPieceType = charToPieceType newPiece
        newBoard = updateBoardWithPromotion (board game) fromPos toPos promotedPieceType
    in if row == promotionRow
       then game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }
       else error "Invalid promotion move"

-- TODO: Write test for invalid move!
handleDisambiguatingMove :: Game -> Char -> Char -> Position -> Game
handleDisambiguatingMove game piece disambiguator (col, row) =
    let pieceType = charToPieceType piece
        boardMap = toPiecePositionsMap (board game)
        possibleFromPositions = findDisambiguatingPiecePositions boardMap pieceType (currentPlayerTurn game) disambiguator
        validFromPos = find (\pos -> canMoveTo pos (col, row) pieceType) possibleFromPositions
    in case validFromPos of
        Just fromPos ->
            let newBoard = updateBoard (board game) fromPos (col, row)
            in game { board = newBoard, currentPlayerTurn = opponent (currentPlayerTurn game) }
        Nothing -> error "No piece found for disambiguating move"

updateBoardWithPromotion :: Chessboard -> Position -> Position -> PieceType -> Chessboard
updateBoardWithPromotion board from to promotedPieceType =
  let movePiece square
        | position square == from = Square from Nothing
        | position square == to   = Square to (Just (Piece promotedPieceType (getColorFromSquare square)))
        | otherwise               = square
      getColorFromSquare square = 
        case piece square of
          Just (Piece _ color) -> color
          Nothing -> error "Invalid board state"
  in map movePiece board

type PiecePositionsMap = [(Position, Piece)]
toPiecePositionsMap :: Chessboard -> PiecePositionsMap
toPiecePositionsMap board = 
  [(position square, piece) | square <- board, Just piece <- [piece square]]

findDisambiguatingPiecePositions :: PiecePositionsMap -> PieceType -> Color -> Char -> [Position]
findDisambiguatingPiecePositions boardMap pieceType color disambiguator =
  [pos | (pos, Piece pt c) <- boardMap, pt == pieceType, c == color, matchesDisambiguator pos disambiguator]

matchesDisambiguator :: Position -> Char -> Bool
matchesDisambiguator (col, row) disambiguator = disambiguator == col || show row == [disambiguator]
