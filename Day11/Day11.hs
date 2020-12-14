module Main where

import Data.Maybe ( catMaybes )
import Control.Monad ( forM_ )
import GHC.Arr

data Seat 
  = Floor
  | Empty
  | Occupied
  deriving (Show, Eq)

type Index = (Int,Int)
type Layout = Array Index Seat

seat :: Char -> Seat
seat '.' = Floor
seat 'L' = Empty
seat '#' = Occupied

char :: Seat -> Char
char Floor = '.'
char Empty = 'L'
char Occupied = '#'

getInput :: FilePath -> IO Layout
getInput fp = do
  lns <- lines <$> readFile fp
  let width  = length (lns !! 0)
      height = length lns
  let seats =
        [((x, y), seat chr) | (y,row) <- zip [0..] lns,
                              (x,chr) <- zip [0..] row ]
  return $ array ((0,0), (width-1, height-1)) seats

printLayout :: Layout -> IO ()
printLayout layout = do
  let (_, (width,height)) = bounds layout
  forM_ [0..height] $ \y -> do
    forM_ [0..width] $ \x -> do
      putStr $ [char $ layout ! (x, y)]
    putStrLn ""

adjacent :: Layout -> Index -> [Seat]
adjacent layout (ix, iy) =
  let (_, (boundX, boundY)) = bounds layout
      ixs = [(x, y) | y <- [iy-1..iy+1],
                      x <- [ix-1..ix+1],
                      0 <= y && y <= boundY,
                      0 <= x && x <= boundX,
                      (x,y) /= (ix,iy) ]
  in map (layout !) ixs

-- Adjecency based on "seeing in a direction"
adjacent2 :: Layout -> Index -> [Seat]
adjacent2 layout (ix,iy) = catMaybes $ map (\d@(dx,dy) -> inDirection (ix+dx, iy+dy) d) directions
  where
    directions = [(dx,dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
    (_, (boundX, boundY)) = bounds layout
    inDirection :: Index -- Inspecting
                -> Index -- Direction
                -> Maybe Seat
    inDirection (x,y) d@(dx,dy) = 
        if 0 <= x && x <= boundX && 0 <= y && y <= boundY
        then case layout ! (x,y) of
               Floor -> inDirection (x+dx, y+dy) d
               seat  -> Just seat
        else Nothing

-- Seat rule or next iteration
rule1 :: Layout -> Index -> Seat
rule1 layout ix =
  let seat = layout ! ix
      adjSeats = adjacent layout ix
  in case seat of
    Empty -> if not (anyOccupied adjSeats)
             then Occupied
             else Empty
    Occupied -> if fourOrMoreOccupied adjSeats
                then Empty
                else Occupied
    Floor -> Floor

rule2 :: Layout -> Index -> Seat
rule2 layout ix =
  let seat = layout ! ix
      adjSeats = adjacent2 layout ix
  in case seat of
    Empty -> if not (anyOccupied adjSeats)
             then Occupied
             else Empty
    Occupied -> if fiveOrMoreOccupied adjSeats
                then Empty
                else Occupied
    Floor -> Floor

anyOccupied :: [Seat] -> Bool
anyOccupied = elem Occupied

fourOrMoreOccupied :: [Seat] -> Bool
fourOrMoreOccupied = (4 <=) . length . filter (Occupied ==)

fiveOrMoreOccupied :: [Seat] -> Bool
fiveOrMoreOccupied = (5 <=) . length . filter (Occupied ==)

iter1 :: Layout -> Layout
iter1 layout = array (bounds layout) (map f $ assocs layout)
  where
      f (ix, _) = (ix, rule1 layout ix)

iter2 :: Layout -> Layout
iter2 layout = array (bounds layout) (map f $ assocs layout)
  where
      f (ix, _) = (ix, rule2 layout ix)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

countOccupied :: Layout -> Int
countOccupied = length . filter (Occupied ==) . elems

main :: IO ()
main = do
  layout <- getInput "input"
  printLayout layout
  putStrLn "--"
  let converged = converge iter1 layout
  printLayout converged
  putStrLn $ "Final umber of occupied seats: " ++ show (countOccupied converged)

  layout2 <- getInput "input"
  printLayout layout2
  putStrLn "--"
  let conv2 = converge iter2 layout2
  printLayout conv2
  putStrLn $ "Final umber of occupied seats: " ++ show (countOccupied conv2)