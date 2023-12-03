{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Graphics.Vty as Vty
import Game

app :: App CounterState () ()
app = App
  { appDraw = \s -> [str ("Count: " ++ show (count s) ++ " Count1: " ++ show (count1 s))]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

handleEvent :: BrickEvent () e -> EventM () CounterState ()
handleEvent (VtyEvent (Vty.EvKey Vty.KUp [])) = modify $ increment
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleEvent _ = return () 

increment :: CounterState -> CounterState
increment s = s { count = count s + 1, count1 = count1 s + 1 }

main :: IO ()
main = do
  let initialState = CounterState { count = 0, count1 = 10 }
  _ <- defaultMain app initialState
  return ()
