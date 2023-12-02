{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import qualified Graphics.Vty as Vty
import Chess

data CounterState = CounterState { count :: Int }

app :: App CounterState () ()
app = App
  { appDraw = \s -> [str ("Count: " ++ show (count s))]
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

handleEvent :: CounterState -> BrickEvent () () -> EventM () CounterState ()
handleEvent s (VtyEvent (Vty.EvKey Vty.KUp [])) =
  modify $ s { count = count s + 1 } -- Increment count on 'Up' arrow press
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) =
  halt -- Halt application on 'q' key press
handleEvent s _ = return () -- Continue application for other events

main :: IO ()
main = do
  let initialState = CounterState { count = 0 }
  _ <- defaultMain app initialState
  return ()