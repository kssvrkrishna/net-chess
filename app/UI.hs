{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Graphics.Vty as Vty
import Game

app :: App Game () ()
app = App
  { appDraw = \s -> [str ("Current player turn: " ++ show(currentPlayerTurn s) ++ " Count: " ++ show (count (counterState s)) ++ " Count1: " ++ show (count1 (counterState s)) ++ "Count_dec: " ++ show (count_dec (counterState1 s)) ++ " Count_dec1: " ++ show (count_dec1 (counterState1 s)))]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

handleEvent :: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (Vty.EvKey Vty.KUp [])) = modify $ increment
handleEvent (VtyEvent (Vty.EvKey Vty.KDown [])) = modify $ decrement
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleEvent _ = return () 

main :: IO ()
main = do
  let initialState = Game {currentPlayerTurn = White, counterState = CounterState { count = 0, count1 = 10 }, counterState1 = CounterState1 { count_dec = 0, count_dec1 = 10 }}
  _ <- defaultMain app initialState
  return ()
