module Main where

import Data.Map ( Map, (!) )
import qualified Data.Map as Map

turn :: Int -- Turn number
     -> Map Int Int  -- Map spoken: turn
     -> ( Int -- Number at turn
        , Map Int Int
        ) 
-- input preamble:
turn 1 _ = (1, Map.empty)
turn 2 _ = (20, Map.fromList [(1,1)])
turn 3 _ = (11, Map.fromList [(1,1),(20,2)])
turn 4 _ = (6, Map.fromList [(1,1),(20,2),(11,3)])
turn 5 _ = (12, Map.fromList [(1,1),(20,2),(11,3),(6,4)])
turn 6 _ = (0, Map.fromList [(1,1),(20,2),(11,3),(6,4),(12,5)])
turn t m = 
  let (n, m') = turn (t - 1) m
  in case m' Map.!? n of
       Nothing -> (0, Map.insert n (t-1) m')
       Just p  -> 
         let speak = t - 1 - p
         in (speak, Map.insert n (t-1) m')

main :: IO ()
main = do
  print $ fst $ turn 2020 Map.empty
  print $ fst $ turn 30000000 Map.empty
