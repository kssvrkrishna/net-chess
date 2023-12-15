module Main (main) where

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented."

import Test.HUnit
import Game
import qualified Data.Text as T
import Control.Exception (evaluate, catch, SomeException)

-- opponent
testOpponentWhite = TestCase (assertEqual "Should return Black for White" Black (Game.opponent White))
testOpponentBlack = TestCase (assertEqual "Should return White for Black" White (Game.opponent Black))

tests = TestList [
    TestLabel "testOpponentWhite" testOpponentWhite,
    TestLabel "testOpponentBlack" testOpponentBlack
    ]

-- getColor
testGetColorWhite = TestCase (assertEqual "Should return White for a white piece" White (getColor (Piece King White)))
testGetColorBlack = TestCase (assertEqual "Should return Black for a black piece" Black (getColor (Piece King Black)))

getColorTests = TestList [
    TestLabel "testGetColorWhite" testGetColorWhite,
    TestLabel "testGetColorBlack" testGetColorBlack
    ]

-- togglePlayerTurn
testTogglePlayerTurn = TestCase $
    let currentGame = Game (0, 0) Nothing White [] (T.pack "input") []
        toggledGame = togglePlayerTurn currentGame
    in
        assertEqual "Should toggle player's turn" Black (currentPlayerTurn toggledGame)

togglePlayerTurnTests = TestList [
    TestLabel "testTogglePlayerTurn" testTogglePlayerTurn
    ]

-- updateGameWithMove
testUpdateGameWithMove = TestCase $
    let currentGame = Game (0, 0) Nothing White [] (T.pack "input") []
        updatedGame = updateGameWithMove currentGame (T.pack "a4")
    in
        assertEqual "Should update move history with a new move" [T.pack "a4"] (moveHistory updatedGame)

updateGameWithMoveTests = TestList [
    TestLabel "testUpdateGameWithMove" testUpdateGameWithMove
    ]

-- textToPosition
testTextToPosition = TestCase $
    let position = textToPosition (T.pack "e2")
    in
        assertEqual "Should convert text to position" ('e', 2) position

textToPositionTests = TestList [
    TestLabel "testTextToPosition" testTextToPosition
    ]

-- findPiece
testFindPiece = TestCase $
    let initialBoard = [ Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 3) (Just (Piece Queen Black)) ]
        foundPiece = findPiece initialBoard ('a', 1)
    in
        assertEqual "Should find the piece at the specified position" (Just (Piece King White)) foundPiece

findPieceTests = TestList [
    TestLabel "testFindPiece" testFindPiece
    ]

-- isValidPawnMove
testIsValidPawnMove = TestList
    [ TestCase $ assertBool "Valid move for White pawn from row 2 to row 3" (isValidPawnMove 2 3 White)
    , TestCase $ assertBool "Valid move for Black pawn from row 7 to row 5" (isValidPawnMove 7 5 Black)
    , TestCase $ assertBool "Invalid move for White pawn from row 5 to row 2" (not $ isValidPawnMove 5 2 White)
    ]

-- charToPieceType
testCharToPieceType = TestList
    [ TestCase $ assertEqual "Convert 'n' to Knight" Knight (charToPieceType 'n')
    , TestCase $ assertEqual "Convert 'b' to Bishop" Bishop (charToPieceType 'b')
    , TestCase $ assertEqual "Convert 'r' to Rook" Rook (charToPieceType 'r')
    , TestCase $ assertEqual "Convert 'q' to Queen" Queen (charToPieceType 'q')
    , TestCase $ assertEqual "Convert 'k' to King" King (charToPieceType 'k')
    -- Add more test cases as needed
    ]

initialBoard = [ Square ('a', 2) (Just (Piece Pawn White))
               , Square ('b', 2) (Just (Piece Pawn White))
               , Square ('c', 3) (Just (Piece Pawn White))
               , Square ('d', 6) (Just (Piece Pawn Black))
               , Square ('e', 7) (Just (Piece Pawn Black))
               -- Add more initial positions as needed for testing
               ]

-- canMoveTo
testCanMoveTo = TestList
    [ TestCase $ assertBool "Valid move for Knight" (canMoveTo ('c', 3) ('e', 4) Knight)
    , TestCase $ assertBool "Valid move for Rook" (canMoveTo ('d', 4) ('d', 8) Rook)
    , TestCase $ assertBool "Valid move for Bishop" (canMoveTo ('b', 2) ('f', 6) Bishop)
    , TestCase $ assertBool "Valid move for Queen" (canMoveTo ('d', 4) ('g', 4) Queen)
    , TestCase $ assertBool "Valid move for King" (canMoveTo ('e', 1) ('d', 2) King)
    , TestCase $ assertBool "Invalid move for piece type" (not $ canMoveTo ('a', 1) ('c', 3) Knight)
    ]

-- validPosition
testValidPosition = TestList
    [ TestCase $ assertBool "Valid position - center" (validPosition ('d', 4))
    , TestCase $ assertBool "Valid position - corner" (validPosition ('h', 1))
    , TestCase $ assertBool "Invalid position - outside bounds" (not $ validPosition ('i', 9))
    ]

-- determineMoveType
testDetermineMoveType = TestList
    [ TestCase $ assertEqual "Pawn Capture" PawnCapture (determineMoveType $ T.pack "cxd4")
    , TestCase $ assertEqual "Piece Capture" PieceCapture (determineMoveType $ T.pack "Nxf7")
    , TestCase $ assertEqual "Pawn Promotion" PawnPromotion (determineMoveType $ T.pack "e8=Q")
    , TestCase $ assertEqual "Disambiguating Move" DisambiguatingMove (determineMoveType $ T.pack "Nbd2")
    ]

-- toPiecePositionsMap
testToPiecePositionsMap = TestList
    [ TestCase $ assertEqual "Empty board should generate an empty map" [] (toPiecePositionsMap [])
    , TestCase $ assertEqual "Should generate PiecePositionsMap from Chessboard" expectedMap (toPiecePositionsMap initialBoard)
    ]
  where
    initialBoard = [Square ('a', 1) (Just (Piece King White)), Square ('b', 2) (Just (Piece Pawn White))]
    expectedMap = [(('a', 1), Piece King White), (('b', 2), Piece Pawn White)]

-- matchesDisambiguator
testMatchesDisambiguator = TestList
    [ TestCase $ assertBool "Matching disambiguator for column" (matchesDisambiguator ('c', 3) 'c')
    , TestCase $ assertBool "Matching disambiguator for row" (matchesDisambiguator ('d', 4) '4')
    , TestCase $ assertBool "Non-matching disambiguator" (not $ matchesDisambiguator ('e', 5) 'f')
    ]

boardMapMock :: PiecePositionsMap
boardMapMock = [ (('a', 1), Piece King White)
               , (('b', 2), Piece Pawn White)
               , (('c', 3), Piece Rook Black)
               , (('d', 4), Piece Queen White)
               ]

-- AllTests
allTests = TestList [
        tests, 
        getColorTests, 
        togglePlayerTurnTests, 
        updateGameWithMoveTests, 
        textToPositionTests, 
        findPieceTests,
        testIsValidPawnMove,
        testCharToPieceType,
        testCanMoveTo,
        testValidPosition,
        testDetermineMoveType,
        testToPiecePositionsMap,
        testMatchesDisambiguator
    ]

main = runTestTT allTests
