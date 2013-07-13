module Stratego where

import Test.HUnit
import Test.QuickCheck

type Board = [[BoardCell]]

type BoardCell = Maybe Square
data Square = Piece PColor PType | NoEntry deriving (Show)
data PColor = Blue | Red deriving (Show)
data PType = Flag | Bomb | Spy | Scout | Miner | General | Marshal deriving (Show)

showBoardCell :: BoardCell -> Char
showBoardCell Nothing   = ' '
showBoardCell (Just a)  = showSquare a      

showSquare :: Square -> Char
showSquare (Piece _ Flag)    = 'F'
showSquare (Piece _ Bomb)    = 'B'
showSquare (Piece _ Spy)     = '1'
showSquare (Piece _ Scout)   = '2'
showSquare (Piece _ Miner)   = '3'
showSquare (Piece _ General) = '9'
showSquare (Piece _ Marshal) = '0'
showSquare NoEntry           = 'W'

readPiece :: Maybe PColor ->Char -> Square
readPiece (Just Blue) 'F' = (Piece Blue Flag)  
readPiece (Just Blue) 'B' = (Piece Blue Bomb)    
readPiece (Just Blue) '1' = (Piece Blue Spy)     
readPiece (Just Blue) '2' = (Piece Blue Scout)   
readPiece (Just Blue) '3' = (Piece Blue Miner)   
readPiece (Just Blue) '9' = (Piece Blue General) 
readPiece (Just Blue) '0' = (Piece Blue Marshal) 

readPiece (Just Red)  'F' = (Piece Red Flag)    
readPiece (Just Red)  'B' = (Piece Red Bomb)    
readPiece (Just Red)  '1' = (Piece Red Spy)     
readPiece (Just Red)  '2' = (Piece Red Scout)   
readPiece (Just Red)  '3' = (Piece Red Miner)   
readPiece (Just Red)  '9' = (Piece Red General) 
readPiece (Just Red)  '0' = (Piece Red Marshal)

readPiece Nothing 'W' = NoEntry
-- Tests

tests = TestList $ map TestCase
  [assertEqual "add tests here"  1 1
  ]

prop_empty c1 = (c1::Int) == c1

runTests = do
  runTestTT tests
  quickCheck prop_empty

-- | For now, main will run our tests.
main :: IO ()
main = runTests