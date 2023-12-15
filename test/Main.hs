module Main (main) where

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented."

import Test.HUnit
import Game
import qualified Data.Text as T

-- Test cases for opponent function
testOpponentWhite = TestCase (assertEqual "Should return Black for White" Black (Game.opponent White))
testOpponentBlack = TestCase (assertEqual "Should return White for Black" White (Game.opponent Black))

tests = TestList [
    TestLabel "testOpponentWhite" testOpponentWhite,
    TestLabel "testOpponentBlack" testOpponentBlack
    ]

-- Test cases for getColor function
testGetColorWhite = TestCase (assertEqual "Should return White for a white piece" White (getColor (Piece King White)))
testGetColorBlack = TestCase (assertEqual "Should return Black for a black piece" Black (getColor (Piece King Black)))

getColorTests = TestList [
    TestLabel "testGetColorWhite" testGetColorWhite,
    TestLabel "testGetColorBlack" testGetColorBlack
    ]

-- Test case for togglePlayerTurn function
testTogglePlayerTurn = TestCase $
    let initialGame = Game (0, 0) Nothing White [] (T.pack "input") []
        toggledGame = togglePlayerTurn initialGame
    in
        assertEqual "Should toggle player's turn" Black (currentPlayerTurn toggledGame)

togglePlayerTurnTests = TestList [
    TestLabel "testTogglePlayerTurn" testTogglePlayerTurn
    ]

-- Test case for updateGameWithMove function
testUpdateGameWithMove = TestCase $
    let initialGame = Game (0, 0) Nothing White [] (T.pack "input") []
        updatedGame = updateGameWithMove initialGame (T.pack "a4")
    in
        assertEqual "Should update move history with a new move" [T.pack "a4"] (moveHistory updatedGame)

updateGameWithMoveTests = TestList [
    TestLabel "testUpdateGameWithMove" testUpdateGameWithMove
    ]

-- Test case for textToPosition function
testTextToPosition = TestCase $
    let position = textToPosition (T.pack "e2")
    in
        assertEqual "Should convert text to position" ('e', 2) position

textToPositionTests = TestList [
    TestLabel "testTextToPosition" testTextToPosition
    ]

-- Test case for updateBoard function
testUpdateBoard = TestCase $
    let initialBoard = [ Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 3) (Just (Piece Queen Black)) ]
        updatedBoard = updateBoard initialBoard ('a', 1) ('b', 2)
    in
        assertEqual "Should update the board correctly" [ Square ('b', 2) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 3) (Just (Piece Queen Black)) ] updatedBoard

updateBoardTests = TestList [
    TestLabel "testUpdateBoard" testUpdateBoard
    ]

-- Combine test lists
allTests = TestList [tests, getColorTests, togglePlayerTurnTests, updateGameWithMoveTests, textToPositionTests, updateBoardTests]

-- Run the tests
main = runTestTT allTests