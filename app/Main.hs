module Main where
import qualified MyLib (someFunc)
import HaskellSay (haskellSay)
import Brick
import Network.Socket

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  haskellSay "Hello, Haskell! You're using a function from another package!"
  MyLib.someFunc
