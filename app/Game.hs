module Game where
import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data CounterState = CounterState { count :: Int, count1 :: Int } deriving (Read, Show)

data Game = Game
  { cursor :: (Int, Int)
  , previous :: Maybe Game
  , counterState :: CounterState
  } deriving (Read, Show)
