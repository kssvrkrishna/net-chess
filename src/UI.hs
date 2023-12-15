{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Graphics.Vty as Vty
import Game
import qualified Data.Char as Char
import Data.List.Split (chunksOf)
import Data.List (intersperse)
import Data.List (sortBy)
import Data.Function (on)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Lens.Micro
import qualified Data.Text as T

-- a4,a5,b3,b5,c3,bxa4,bxa4, ba6, qb3, Bxe2, Bxe2, Nc6, Nf3, Nf6, Nd4, Nxd4, cxd4
drawChessboard :: Chessboard -> [Widget ()]
drawChessboard board =
  let renderSquare (Square pos piece) =
        case piece of
          Nothing -> str "   "  -- Empty square, three spaces
          Just (Piece pieceType color) ->
            let pieceSymbol =
                  case (pieceType, color) of
                    (Pawn, White) -> "♟"
                    (Rook, White) -> "♜"
                    (Knight, White) -> "♞"
                    (Bishop, White) -> "♝"
                    (Queen, White) -> "♛"
                    (King, White) -> "♚"
                    (Pawn, Black) -> "♙"
                    (Rook, Black) -> "♖"
                    (Knight, Black) -> "♘"
                    (Bishop, Black) -> "♗"
                    (Queen, Black) -> "♕"
                    (King, Black) -> "♔"
            in str (" " ++ pieceSymbol ++ " ")  -- Padding spaces around the piece

      renderRow :: [Square] -> Widget ()
      renderRow row = hBox $ intersperse (str "│") (map renderSquare row)

      -- Convert each row to a Widget, separating rows by horizontal lines
      rearrangedRows = reverse $ chunksOf 8 board  -- Arrange the board into rows and reverse the order
      renderedRows = map renderRow rearrangedRows
      horizontalLine = hBox (replicate 8 (str "────"))  -- Horizontal line for separation

      -- Insert horizontal lines between each row
      boardWithLines = intersperse horizontalLine renderedRows

  in [vBox (boardWithLines)]  -- Assemble the rows with lines into the final widget

drawHelp :: Game -> Widget ()
drawHelp game =
  [ "Move: Algebraic notation"
  , "Player turn: " ++ show (currentPlayerTurn game)
  , "Quit: Ctrl + C"
  ]
  & unlines
  & str
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)  -- Increased the available size of the help box

drawInputChars :: Game -> Widget ()
drawInputChars game =
  let input = T.unpack (inputChars game)
      horizontalLine = hBox (replicate 8 (str "────")) -- Define the horizontal line
      minWidth = 60  -- Set the minimum width
  in vBox [ horizontalLine  -- Add the horizontal line above the widget
          , str ("Input: " ++ input)
            & padLeftRight 5
            & hLimit minWidth
            & borderWithLabel (str " Enter your move ")
            & withBorderStyle unicodeBold
          ]

drawMoveHistory :: [T.Text] -> Widget ()
drawMoveHistory moves =
    let (whiteMoves, blackMoves) = splitMoves moves
        whiteColumn = vBox $ map str $ "White" : whiteMoves
        blackColumn = vBox $ map str $ "Black" : blackMoves
    in hBox [whiteColumn <+> vBorder <+> blackColumn]

splitMoves :: [T.Text] -> ([String], [String])
splitMoves moves =
    let indexedMoves = zip [1..] moves
        whiteMoves = [T.unpack m | (i, m) <- indexedMoves, odd i]
        blackMoves = [T.unpack m | (i, m) <- indexedMoves, even i]
    in (whiteMoves, blackMoves)

drawUI :: Game -> Widget ()
drawUI game =
  vBox [drawHelp (game), vBox (drawChessboard (board game)),drawInputChars game, drawMoveHistory (moveHistory game)]

app :: App Game () ()
app = App
  { appDraw = \s -> [drawUI s]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

handleEvent :: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt
handleEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  input <- gets inputChars
  game <- get
  let newGame = case T.length input of
                  2 -> pawnMove game input
                  3 -> handleThreeLengthMove game input  
                  4 -> handleFourLengthMove game input 
                  _ -> game { inputChars = T.empty } 
  let newGame' = updateGameWithMove newGame input                
  put newGame' { inputChars = T.empty }  
  return ()
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar c) [])) = do
  modify $ \s -> s { inputChars = T.snoc (inputChars s) c }
  return ()
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl])) = do
  modify $ \s -> s { inputChars = T.empty }
  return ()
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl])) = do
  maybePrev <- gets previous
  case maybePrev of
    Just prev -> put prev  -- Revert to previous state if available
    Nothing -> return ()
handleEvent _ = return ()


main :: IO ()
main = do
  let initialState = Game {previous = Nothing, 
  inputChars = T.empty,
  board = initialChessBoard, 
  currentPlayerTurn = White,
  moveHistory = [],
  capturedPieces =[]}
  _ <- defaultMain app initialState
  return ()
