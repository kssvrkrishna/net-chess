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

-- updateBoard
testUpdateBoard = TestCase $
    let initialBoard = [ Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 3) (Just (Piece Queen Black)) ]
        updatedBoard = updateBoard initialBoard ('a', 1) ('b', 2)
    in
        assertEqual "Should update the board correctly" [ Square ('b', 2) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 3) (Just (Piece Queen Black)) ] updatedBoard

updateBoardTests = TestList [
    TestLabel "testUpdateBoard" testUpdateBoard
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

-- pawnMove: Valid
testPawnMoveValid = TestCase $
    let currentGame = Game (0, 0) Nothing White [ Square ('a', 2) (Just (Piece Pawn White)), Square ('b', 3) Nothing, Square ('c', 4) Nothing ] (T.pack "input") []
        updatedGame = pawnMove currentGame (T.pack "a3")
        expectedBoard = [ Square ('a', 3) (Just (Piece Pawn White)), Square ('b', 3) Nothing, Square ('c', 4) Nothing ]
    in
        assertEqual "Should update the board correctly for a valid move" expectedBoard (board updatedGame)

-- pawnMove: Invalid
testPawnMoveInvalid = TestCase $ do
    let currentGame = Game (0, 0) Nothing White [ Square ('a', 2) (Just (Piece Pawn White)), Square ('b', 3) Nothing, Square ('c', 4) Nothing ] (T.pack "input") []
        expectedErrorMessage = "No pawn found for move"
        action = evaluate (pawnMove currentGame (T.pack "d3"))
    result <- catch (action >> return Nothing) (\e -> return (Just (show (e :: SomeException))))
    assertEqual "Should throw an error for an invalid move" (Just expectedErrorMessage) result

pawnMoveTests = TestList [
    TestLabel "testPawnMoveValid" testPawnMoveValid,
    TestLabel "testPawnMoveInvalid" testPawnMoveInvalid
    ]

-- findPawnPosition
testFindPawnPosition = TestCase $
    let initialBoard = [ Square ('a', 2) (Just (Piece Pawn White)), Square ('b', 3) (Just (Piece Pawn Black)), Square ('c', 4) (Just (Piece Rook White)) ]
        result = findPawnPosition initialBoard White 'a' 2
        expected = Just ('a', 2)
    in
        assertEqual "Should find the pawn's position" expected result

findPawnPositionTests = TestList [
    TestLabel "testFindPawnPosition" testFindPawnPosition
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

-- findMovablePiece
testFindMovablePiece = TestList
    [ TestCase $ assertEqual "Find movable piece - White Pawn at ('b', 2)" (Just ('b', 2)) (findMovablePiece initialBoard Pawn White ('c', 3))
    , TestCase $ assertEqual "Find movable piece - Black Pawn at ('e', 7)" (Just ('e', 7)) (findMovablePiece initialBoard Pawn Black ('d', 6))
    , TestCase $ assertEqual "No movable piece for Rook" Nothing (findMovablePiece initialBoard Rook White ('a', 1))
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

-- lineMoves
testLineMoves = TestList
    [ TestCase $ assertEqual "Line moves - horizontal" [('c', 4), ('d', 4), ('e', 4), ('f', 4)] (lineMoves ('c', 4) [(1, 0), (1, 0), (1, 0), (1, 0)])
    , TestCase $ assertEqual "Line moves - vertical" [('d', 5), ('d', 6), ('d', 7), ('d', 8)] (lineMoves ('d', 5) [(0, 1), (0, 1), (0, 1), (0, 1)])
    , TestCase $ assertEqual "Line moves - diagonal" [('c', 3), ('b', 2), ('a', 1)] (lineMoves ('c', 3) [(-1, -1), (-1, -1), (-1, -1)])
    ]

-- adjacentMoves
testAdjacentMoves = TestList
    [ TestCase $ assertEqual "Adjacent moves - center" [('c', 4), ('d', 4), ('e', 4), ('c', 5), ('e', 5), ('c', 3), ('d', 3), ('e', 3)] (adjacentMoves ('d', 4))
    , TestCase $ assertEqual "Adjacent moves - corner" [('a', 1), ('b', 1), ('a', 2), ('b', 2)] (adjacentMoves ('a', 1))
    ]

-- knightMoves
testKnightMoves = TestList
    [ TestCase $ assertEqual "Knight moves - center" [('b', 3), ('c', 2), ('e', 2), ('f', 3), ('b', 5), ('c', 6), ('e', 6), ('f', 5)] (knightMoves ('d', 4))
    , TestCase $ assertEqual "Knight moves - corner" [('b', 2), ('c', 3)] (knightMoves ('a', 1))
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
-- findDisambiguatingPiecePositions
testFindDisambiguatingPiecePositions = TestList
    [ TestCase $ assertEqual "Disambiguating piece positions: Matching column" [('b', 2), ('d', 4)] (findDisambiguatingPiecePositions boardMapMock Pawn White 'b')
    , TestCase $ assertEqual "Disambiguating piece positions: Matching row" [('a', 1)] (findDisambiguatingPiecePositions boardMapMock King White '1')
    , TestCase $ assertEqual "Disambiguating piece positions: No match" [] (findDisambiguatingPiecePositions boardMapMock Queen White 'z')
    ]

-- updateBoardWithPromotion
testUpdateBoardWithPromotion = TestList
    [ TestCase $ assertEqual "Update board with promotion - Pawn to Queen" 
        [Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 8) (Just (Piece Queen Black))]
        (updateBoardWithPromotion initialBoard ('c', 7) ('c', 8) Queen)
    , TestCase $ assertEqual "Update board with promotion - Pawn to Rook" 
        [Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 8) (Just (Piece Rook Black))]
        (updateBoardWithPromotion initialBoard ('c', 7) ('c', 8) Rook)
    ]
  where
    initialBoard = [Square ('a', 1) (Just (Piece King White)), Square ('b', 2) Nothing, Square ('c', 7) (Just (Piece Pawn Black))]

-- handleDisambiguatingMove
testHandleDisambiguatingMove = TestList
    [ TestCase $ assertEqual "Handle Disambiguating Move: Valid"
        expectedGame
        (handleDisambiguatingMove currentGame 'N' 'b' ('c', 3))
    ]
  where
    currentGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = White
        , board = [ Square ('a', 1) (Just (Piece Knight White))
                  , Square ('b', 3) (Just (Piece Pawn Black))
                  , Square ('c', 4) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }
    expectedGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = Black
        , board = [ Square ('a', 1) Nothing
                  , Square ('b', 3) (Just (Piece Knight White))
                  , Square ('c', 3) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }

-- handlePawnPromotion
testHandlePawnPromotion = TestList
    [ TestCase $ assertEqual "Handle Pawn Promotion: Valid"
        expectedGame
        (handlePawnPromotion initialGame ('d', 8) 'Q')
    ]
  where
    initialGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = White
        , board = [ Square ('d', 7) (Just (Piece Pawn White))
                  , Square ('a', 2) (Just (Piece Pawn Black))
                  , Square ('e', 8) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }
    expectedGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = Black
        , board = [ Square ('d', 8) (Just (Piece Queen White))
                  , Square ('a', 2) (Just (Piece Pawn Black))
                  , Square ('e', 8) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }

-- handlePieceCapture
testHandlePieceCapture = TestList
    [ TestCase $ assertEqual "Handle Piece Capture: Valid"
        expectedGame
        (handlePieceCapture currentGame 'N' ('e', 7))
    ]
  where
    currentGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = Black
        , board = [ Square ('a', 1) (Just (Piece Pawn White))
                  , Square ('b', 2) Nothing
                  , Square ('c', 3) (Just (Piece Knight Black))
                  , Square ('d', 4) Nothing
                  , Square ('e', 5) Nothing
                  , Square ('f', 6) Nothing
                  , Square ('g', 7) Nothing
                  , Square ('h', 8) Nothing
                  , Square ('e', 7) (Just (Piece Bishop Black))
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }
    expectedGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = White
        , board = [ Square ('a', 1) (Just (Piece Pawn White))
                  , Square ('b', 2) Nothing
                  , Square ('c', 3) Nothing
                  , Square ('d', 4) Nothing
                  , Square ('e', 5) Nothing
                  , Square ('f', 6) Nothing
                  , Square ('g', 7) Nothing
                  , Square ('h', 8) Nothing
                  , Square ('e', 7) (Just (Piece Knight Black))
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }

-- handlePawnCapture
testHandlePawnCapture = TestList
    [ TestCase $ assertEqual "Handle Pawn Capture: Valid"
        expectedGame
        (handlePawnCapture currentGame 'd' ('e', 4))
    ]
  where
    currentGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = Black
        , board = [ Square ('a', 1) Nothing
                  , Square ('b', 2) (Just (Piece Pawn White))
                  , Square ('c', 3) Nothing
                  , Square ('d', 4) (Just (Piece Pawn Black))
                  , Square ('e', 5) Nothing
                  , Square ('f', 6) Nothing
                  , Square ('g', 7) (Just (Piece Pawn White))
                  , Square ('h', 8) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }
    expectedGame = Game
        { cursor = (0, 0)
        , previous = Nothing
        , currentPlayerTurn = White
        , board = [ Square ('a', 1) Nothing
                  , Square ('b', 2) (Just (Piece Pawn White))
                  , Square ('c', 3) Nothing
                  , Square ('d', 4) Nothing
                  , Square ('e', 5) Nothing
                  , Square ('f', 6) Nothing
                  , Square ('g', 7) (Just (Piece Pawn White))
                  , Square ('h', 8) Nothing
                  ]
        , inputChars = T.pack "input"
        , moveHistory = []
        }

-- AllTests
allTests = TestList [
        tests, 
        getColorTests, 
        togglePlayerTurnTests, 
        updateGameWithMoveTests, 
        textToPositionTests, 
        updateBoardTests, 
        findPieceTests,
        pawnMoveTests,
        findPawnPositionTests,
        testIsValidPawnMove,
        testCharToPieceType,
        testFindMovablePiece,
        testCanMoveTo,
        testLineMoves,
        testAdjacentMoves,
        testKnightMoves,
        testValidPosition,
        testDetermineMoveType,
        testToPiecePositionsMap,
        testMatchesDisambiguator,
        testFindDisambiguatingPiecePositions,
        testUpdateBoardWithPromotion,
        testHandleDisambiguatingMove,
        testHandlePawnPromotion,
        testHandlePieceCapture,
        testHandlePawnCapture
    ]

main = runTestTT allTests
