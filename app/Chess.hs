module Chess where

data Color = Black | White deriving (Eq, Show)

opponent :: Color -> Color
opponent White = Black
opponent Black = White

data PieceType = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq, Show)

data Piece = Piece PieceType Color deriving (Eq, Show)

data Position = Position Int Int deriving (Eq, Show)

type Board = [(Position, Piece Color)]

data Move = Move Position Position deriving (Eq, Show) 

data Game = Game { 
    board :: Board,
    currentPlayer :: Color,
}

getColor :: Piece Color -> Color
getColor (Piece _ color) = color

initialBoard :: Board
initialBoard = [
    (Position 1 1, Piece Rook White), (Position 2 1, Piece Knight White), 
    (Position 3 1, Piece Bishop White), (Position 4 1, Piece Queen White), 
    (Position 5 1, Piece King White), (Position 6 1, Piece Bishop White), 
    (Position 7 1, Piece Knight White), (Position 8 1, Piece Rook White),
    (Position 1 2, Piece Pawn White), (Position 2 2, Piece Pawn White), r
    (Position 3 2, Piece Pawn White), (Position 4 2, Piece Pawn White), 
    (Position 5 2, Piece Pawn White), (Position 6 2, Piece Pawn White), 
    (Position 7 2, Piece Pawn White), (Position 8 2, Piece Pawn White),

    (Position 1 8, Piece Rook Black), (Position 2 8, Piece Knight Black), 
    (Position 3 8, Piece Bishop Black), (Position 4 8, Piece Queen Black), 
    (Position 5 8, Piece King Black), (Position 6 8, Piece Bishop Black), 
    (Position 7 8, Piece Knight Black), (Position 8 8, Piece Rook Black),
    (Position 1 7, Piece Pawn Black), (Position 2 7, Piece Pawn Black), 
    (Position 3 7, Piece Pawn Black), (Position 4 7, Piece Pawn Black), 
    (Position 5 7, Piece Pawn Black), (Position 6 7, Piece Pawn Black), 
    (Position 7 7, Piece Pawn Black), (Position 8 7, Piece Pawn Black)
]

initialGame :: Game
initialGame = Game {
    board = initialBoard,
    currentPlayer = White
}

makeMove :: Board -> Move -> Board
makeMove board (Move fromPos toPos) =
    let
        pieceAtFrom = findPieceAtPosition fromPos board
        pieceAtTo = findPieceAtPosition toPos board
        moveIsValid = case pieceAtTo of
            Nothing -> True
            Just (_, piece) -> case pieceAtFrom of
                Just (Piece _ color) -> color /= getColor piece 
                Nothing -> False
    in
        case (pieceAtFrom, moveIsValid) of
            (Just piece, True) ->
                updatedBoard = filter (\(pos, _) -> pos /= fromPos) board 
                (toPos, piece) : updatedBoard 
            _ ->
                board 

findPieceAtPosition :: Position -> Board -> Maybe (Position, Piece Color)
findPieceAtPosition pos board =
    lookup pos board            