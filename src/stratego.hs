module Stratego where

import Test.HUnit
import Test.QuickCheck

type Board = [[BoardCell]]

type BoardCell = Maybe Square
data Square = Piece PColor PType | NoEntry deriving (Show)
data PColor = Blue | Red deriving (Show)
data PType = Flag | Bomb | Spy | Scout | Miner | General | Marshal deriving (Show)



showBoardCell :: BoardCell -> Char
showBoardCell = maybe ' ' showSquare    

showSquare :: Square -> Char
showSquare (Piece _ Flag)    = 'F'
showSquare (Piece _ Bomb)    = 'B'
showSquare (Piece _ Spy)     = '1'
showSquare (Piece _ Scout)   = '2'
showSquare (Piece _ Miner)   = '3'
showSquare (Piece _ General) = '9'
showSquare (Piece _ Marshal) = '0'
showSquare NoEntry           = 'W'

readSquare :: Maybe PColor ->Char -> Square
readSquare (Just Blue) 'F' = (Piece Blue Flag)  
readSquare (Just Blue) 'B' = (Piece Blue Bomb)    
readSquare (Just Blue) '1' = (Piece Blue Spy)     
readSquare (Just Blue) '2' = (Piece Blue Scout)   
readSquare (Just Blue) '3' = (Piece Blue Miner)   
readSquare (Just Blue) '9' = (Piece Blue General) 
readSquare (Just Blue) '0' = (Piece Blue Marshal) 

readSquare (Just Red)  'F' = (Piece Red Flag)    
readSquare (Just Red)  'B' = (Piece Red Bomb)    
readSquare (Just Red)  '1' = (Piece Red Spy)     
readSquare (Just Red)  '2' = (Piece Red Scout)   
readSquare (Just Red)  '3' = (Piece Red Miner)   
readSquare (Just Red)  '9' = (Piece Red General) 
readSquare (Just Red)  '0' = (Piece Red Marshal)

readSquare Nothing 'W' = NoEntry
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