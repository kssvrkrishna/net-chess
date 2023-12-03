module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data CounterState = CounterState { count :: Int, count1 :: Int } deriving (Read, Show)
data CounterState1 = CounterState1 { count_dec :: Int, count_dec1 :: Int } deriving (Read, Show)

data Color = Black | White deriving (Eq, Show, Read)

data Game = Game
  { cursor :: (Int, Int)
  , previous :: Maybe Game
  , counterState :: CounterState
  , counterState1 :: CounterState1
  , currentPlayerTurn :: Color
  } deriving (Read, Show)

increment :: Game -> Game
increment game =
  let updatedCounterState = (counterState game) { count = count (counterState game) + 1
                                               , count1 = count1 (counterState game) + 1
                                               }
      updatedGame = game { counterState = updatedCounterState }
  in togglePlayerTurn updatedGame

decrement :: Game -> Game
decrement game =
  let updatedCounterState = (counterState1 game) { count_dec = count_dec (counterState1 game) - 1
                                               , count_dec1 = count_dec1 (counterState1 game) - 1
                                               }
      updatedGame = game { counterState1 = updatedCounterState }
  in togglePlayerTurn updatedGame

opponent :: Color -> Color
opponent White = Black
opponent Black = White

togglePlayerTurn :: Game -> Game
togglePlayerTurn game = game { currentPlayerTurn = opponent (currentPlayerTurn game) }
