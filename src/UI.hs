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

-- drawChessboard :: Chessboard -> [Widget ()]
-- drawChessboard board =
--   let renderSquare (Square pos piece) =
--         let (c, i) = pos
--         in str (" " ++ [c] ++ show i ++ " ")  -- Show the position with proper formatting

--       renderRow :: [Square] -> Widget ()
--       renderRow row = hBox $ intersperse (str "│") (map renderSquare row)

--       -- Convert each row to a Widget, separating rows by horizontal lines
--       rearrangedRows = reverse $ chunksOf 8 board  -- Arrange the board into rows and reverse the order
--       renderedRows = map renderRow rearrangedRows
--       horizontalLine = hBox (replicate 8 (str "───"))  -- Horizontal line for separation

--   in [vBox (intersperse (str "\n") (renderedRows ++ [horizontalLine]))]  -- Separate rows with newline

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
  , "Counter state: " ++ show (count (counterState game))
  , "Player turn: " ++ show (currentPlayerTurn game)
  , "Quit: Q key"
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
            & borderWithLabel (str " Current Input ")
            & withBorderStyle unicodeBold
          ]

drawUI :: Game -> Widget ()
drawUI game =
  vBox [drawHelp (game), vBox (drawChessboard (board game)), drawInputChars game]

app :: App Game () ()
app = App
  { appDraw = \s -> [drawUI s] -- drawChessboard (board s)
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
                  3 -> handleThreeLengthMove game input  -- Implement this function as per your game logic
                  4 -> handleFourLengthMove game input -- Implement this function as per your game logic
                  _ -> game { inputChars = T.empty }  -- Clear input on invalid move
  put newGame { inputChars = T.empty }  -- Clear inputChars after processing the move
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
  let initialState = Game {previous = Nothing, inputChars = T.empty, board = initialChessBoard, currentPlayerTurn = White, counterState = CounterState { count = 0, count1 = 10 }, counterState1 = CounterState1 { count_dec = 0, count_dec1 = 10 }}
  _ <- defaultMain app initialState
  return ()
