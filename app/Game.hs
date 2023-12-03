module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data CounterState = CounterState { count :: Int, count1 :: Int } deriving (Read, Show)
data CounterState1 = CounterState1 { count_dec :: Int, count_dec1 :: Int } deriving (Read, Show)

data Game = Game
  { cursor :: (Int, Int)
  , previous :: Maybe Game
  , counterState :: CounterState
  , counterState1 :: CounterState1
  } deriving (Read, Show)

increment :: Game -> Game
increment s = s { counterState = CounterState { count = count (counterState s) + 1, count1 = count1 (counterState s) + 1 } }

decrement :: Game -> Game
decrement s = s { counterState1 = CounterState1 { count_dec = count_dec (counterState1 s) - 1, count_dec1 = count_dec1 (counterState1 s) - 1 } }
