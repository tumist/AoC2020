{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Map ( Map, (!))
import qualified Data.Map as Map

-- Part 1: Game simulator

data GameState = GameState
  { lastSpoken :: Int
    -- Map k: Number
    --     v: Turns it was spoken in descending order
  , spokenTurns :: Map Int [Int]
  , nextTurn :: Int -- next turn number
  } 
  deriving (Show)

initialState :: [Int] -> GameState
initialState nums =
  GameState 
  { lastSpoken = last nums
  , spokenTurns = Map.fromList $ map (\(n,t) -> (n,[t])) $ zip nums [1..]
  , nextTurn = 1 + length nums
  }

doTurn :: GameState -> GameState
doTurn st@GameState{..} = 
  let lastSpokenTurns = spokenTurns ! lastSpoken
      nextNumber =
        case lastSpokenTurns of
          -- If it was the first time it was spoken, the current player says 0
          [_] -> 0
          -- Else, the next number is the difference of turn numbers in the last two
          -- times it was previously spoken
          [a,b] -> a - b
      turnAdd :: Int -> Maybe [Int] -> Maybe [Int]
      turnAdd trn Nothing = Just [trn]
      turnAdd trn (Just [p]) = Just [trn,p]
      turnAdd trn (Just [p,_]) = Just [trn,p]
  in st { lastSpoken = nextNumber
        , spokenTurns = Map.alter (turnAdd nextTurn) nextNumber spokenTurns
        , nextTurn = nextTurn + 1 }

nthSpoken :: Int -> GameState -> Int
nthSpoken n = lastSpoken . until (\st -> nextTurn st > n) doTurn 

-- Part 2:
-- Our game simulator works takes 21 ms for 2020 turns,
-- but two minutes for 30000000.
-- The alternative solution takes about 1 minute.

main :: IO ()
main = do
  let state = initialState [1,20,11,6,12,0]
  print $ nthSpoken 2020 state
  print $ nthSpoken 30000000 state
