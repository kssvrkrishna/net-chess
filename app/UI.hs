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

drawChessboard :: Chessboard -> [Widget ()]
drawChessboard board =
  let renderSquare (Square _ piece) =
        case piece of
          Nothing -> str "   "  -- Empty square, three spaces
          Just (Piece pieceType color) ->
            let pieceSymbol =
                  case (pieceType, color) of
                    (Pawn, White) -> "♙"
                    (Rook, White) -> "♖"
                    (Knight, White) -> "♘"
                    (Bishop, White) -> "♗"
                    (Queen, White) -> "♕"
                    (King, White) -> "♔"
                    (Pawn, Black) -> "♟"
                    (Rook, Black) -> "♜"
                    (Knight, Black) -> "♞"
                    (Bishop, Black) -> "♝"
                    (Queen, Black) -> "♛"
                    (King, Black) -> "♚"
            in str (" " ++ pieceSymbol ++ " ")  -- Padding spaces around the piece

      renderRow :: [Square] -> Widget ()
      renderRow row = hBox $ intersperse (str "│") (map renderSquare row)

      -- Convert each row to a Widget, separating rows by horizontal lines
      rearrangedRows = reverse $ chunksOf 8 board  -- Reverse the rows
      renderedRows = map renderRow rearrangedRows
      horizontalLine = hBox (replicate 8 (str "───"))  -- Horizontal line for separation

  in [vBox (intersperse (str "\n") (horizontalLine : renderedRows))]  -- Separate rows with newline

drawHelp :: CounterState -> Widget ()
drawHelp counterState =
  [ "Move: Algebraic notation"
  , "Player turn: " ++ show(count counterState)
  , "Quit: Q key"
  ]
  & unlines
  & str
  & padLeftRight 1
  & borderWithLabel (str " Help ")
  & withBorderStyle unicodeBold
  & setAvailableSize (31, 12)

drawUI :: Game -> Widget ()
drawUI game =
  vBox [drawHelp (counterState game), vBox (drawChessboard (board game))]

app :: App Game () ()
app = App
  { appDraw = \s -> [drawUI s] -- drawChessboard (board s)
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

handleEvent :: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (Vty.EvKey Vty.KUp [])) = modify $ increment
handleEvent (VtyEvent (Vty.EvKey Vty.KDown [])) = modify $ decrement
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  input <- gets inputChars
  if T.length input == 3  -- Check if input is in algebraic notation format
    then modify $ decrement
  else if T.length input == 4  -- Check if input is in algebraic notation format
    then modify $ increment
  else do
    modify $ \s -> s { inputChars = T.empty }
    return ()  -- Invalid input length, do nothing
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar c) [])) = do
  modify $ \s -> s { inputChars = T.snoc (inputChars s) c } -- Append the pressed key to inputChars
  return ()
handleEvent _ = return ()

main :: IO ()
main = do
  let initialState = Game {inputChars = T.empty, board = initialChessBoard, currentPlayerTurn = White, counterState = CounterState { count = 0, count1 = 10 }, counterState1 = CounterState1 { count_dec = 0, count_dec1 = 10 }}
  _ <- defaultMain app initialState
  return ()
