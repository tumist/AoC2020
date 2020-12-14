{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (Left, Right)

data Direction
  = North
  | East
  | South
  | West
  | Forward
  | Left
  | Right
  deriving Eq

data Instruction = Instruction Direction Int deriving Eq

instance Show Direction where
  show North = "N"
  show East = "E"
  show South = "S"
  show West = "W"
  show Forward = "F"
  show Left = "L"
  show Right = "R"

instance Read Direction where
  readsPrec _ input = do
    case input of
      "N" -> pure (North, "")
      "E" -> pure (East, "")
      "S" -> pure (South, "")
      "W" -> pure (West, "")
      "F" -> pure (Forward, "")
      "L" -> pure (Left, "")
      "R" -> pure (Right, "")

instance Show Instruction where
  show (Instruction dir amt) = show dir ++ show amt

instance Read Instruction where
  readsPrec _ input =
    let (dir,amt) = splitAt 1 input
    in pure (Instruction (read dir) (read amt), "")

data State = State { dir :: Direction,
                     x :: Int,
                     y :: Int } deriving (Show, Eq)

data Waypoint = Waypoint { wx :: Int, wy :: Int }
  deriving (Show, Eq)

data State2 = State2 { waypoint :: Waypoint
                     , x2 :: Int
                     , y2 :: Int } deriving (Show, Eq)

initialState :: State
initialState = State East 0 0 

initialState2 :: State2
initialState2 = State2 (Waypoint 10 1) 0 0

getInput :: FilePath -> IO [Instruction]
getInput fp = do
  file <- readFile fp
  let lns = lines file
  return $ map read lns

left :: Direction -> Direction
left North = West
left West = South
left South = East
left East = North

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

turn :: Int -> Direction -> State -> State
turn 0 _ st = st
turn amt Right st@State{dir} = turn (amt - 90) Right (st { dir = right dir})
turn amt Left  st@State{dir} = turn (amt - 90) Left (st { dir = left dir})

go :: Direction -> Int -> State -> State
go North amt (State dir x y) = State dir x (y + amt)
go South amt (State dir x y) = State dir x (y - amt)
go East amt (State dir x y) = State dir (x + amt) y
go West amt (State dir x y) = State dir (x - amt) y

applyInstr :: State -> Instruction -> State
applyInstr st@State{dir} (Instruction Forward amt) = go dir amt st
applyInstr st (Instruction Left amt) = turn amt Left st
applyInstr st (Instruction Right amt) = turn amt Right st
applyInstr st (Instruction dir amt) = go dir amt st

applyInstr2 :: State2 -> Instruction -> State2
applyInstr2 st@State2{waypoint} (Instruction Forward amt) =
  State2 { waypoint = waypoint
         , x2 = x2 st + (wx waypoint * amt)
         , y2 = y2 st + (wy waypoint * amt) }
applyInstr2 st@State2{waypoint} instr = st { waypoint = applyWaypoint instr waypoint }

applyWaypoint :: Instruction -> Waypoint -> Waypoint
applyWaypoint (Instruction North amt) wp@Waypoint{wy} = wp { wy = wy + amt}
applyWaypoint (Instruction South amt) wp@Waypoint{wy} = wp { wy = wy - amt}
applyWaypoint (Instruction East amt)  wp@Waypoint{wx} = wp { wx = wx + amt}
applyWaypoint (Instruction West amt)  wp@Waypoint{wx} = wp { wx = wx - amt}
applyWaypoint (Instruction _ 0) wp = wp
-- Right turn: Waypoint 2 1 becomes Waypoint 1 -2 
applyWaypoint (Instruction Right amt) wp =
  applyWaypoint (Instruction Right (amt - 90)) $ wp { wx = wy wp, wy = negate (wx wp) }
-- Left turn: Waypoint 2 1 becomes -1 2
applyWaypoint (Instruction Left  amt) wp = 
  applyWaypoint (Instruction Left (amt - 90)) $ wp { wx = negate (wy wp), wy = wx wp }

applyInstructions :: State -> [Instruction] -> State
applyInstructions = foldl applyInstr

applyInstructions2 :: State2 -> [Instruction] -> State2
applyInstructions2 = foldl applyInstr2

distance :: Int -> Int -> Int
distance x y = abs x + abs y

main :: IO ()
main = do
  instrs <- getInput "input"
  -- Part 1
  print instrs
  let end = applyInstructions initialState instrs
  print (distance (x end) (y end))
  -- Part 2 
  let end2 = applyInstructions2 initialState2 instrs
  print end2
  print (distance (x2 end2) (y2 end2))