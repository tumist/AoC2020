{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( forM )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C8
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Attoparsec.ByteString.Char8 ( Parser, char, skipSpace, endOfLine, string, decimal )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Maybe ( catMaybes )
import Data.List ( nub,sortOn,  sortBy, delete )
import Data.Sort ( sortOn )
import Debug.Trace

data Field
  = Field { name :: ByteString
          , valueRange :: Set Int
  } deriving (Show, Eq)

type Ticket = [Int]

parseField :: Parser Field
parseField = do
  fieldName <- AP.takeTill (== ':')
  char ':' >> skipSpace 
  values <- Set.unions <$> parseRange `AP.sepBy` " or "
  endOfLine
  pure $ Field fieldName values
  where
    parseRange :: Parser (Set Int)
    parseRange = do
      lower <- AP.decimal
      char '-'
      upper <- AP.decimal
      pure $ Set.fromAscList [lower..upper]

parseTicket :: Parser Ticket
parseTicket = do
  values <- decimal `AP.sepBy` ","
  endOfLine 
  pure values

parseInput :: Parser ([Field], Ticket, [Ticket])
parseInput = do
  fields <- AP.many1 parseField
  endOfLine 
  string "your ticket:" >> endOfLine
  yourTicket <- parseTicket
  endOfLine 
  string "nearby tickets:" >> endOfLine 
  nearbyTickets <- AP.many1 parseTicket
  pure (fields, yourTicket, nearbyTickets)

mergeFields :: [Field] -> Set Int
mergeFields = Set.unions . map valueRange

errorRate :: [Field] -> Ticket -> Int
errorRate fields ticketVals = 
  let allowedValues = mergeFields fields
      valSet = Set.fromList ticketVals
  in sum $ Set.difference valSet allowedValues

isValid :: [Field] -> Ticket -> Bool
isValid fields ticketVals =
  let allowedValues = mergeFields fields
      valSet = Set.fromList ticketVals
  in Set.difference valSet allowedValues == Set.empty

columnsToFields :: [(Int, [Field])] -> [(Int, Field)]
columnsToFields [] = []
columnsToFields columns =
  -- There will always be a column with only one field possible
  -- Wouldn't have solved this without tritlo
  let sorted = sortOn (length . snd) columns
      match@(c, [field]) = head sorted
  in (c, field) : columnsToFields (map (deleteField field) (delete match columns))
  where
    deleteField :: Field -> (Int, [Field]) -> (Int, [Field])
    deleteField f (c, flds) = (c, delete f flds)

main :: IO ()
main = do
  Right (fields, your, nearby) <- AP.parseOnly parseInput <$> C8.readFile "input"
  -- Part 1
  print $ sum $ map (errorRate fields) nearby
  
  -- Part 2
  let validNearby = filter (isValid fields) nearby
      column n = map (!! n) (your:validNearby)
      ticketLen = length your -- We may assume all the tickets have same amount of numbers

  -- [(column number, [fields that fit])]
  columnPossibilities <- forM [0..(ticketLen-1)] $ \c -> do
    putStrLn $ "Column " ++ show c ++ " all fit in the following fields:"
    matchFields <- forM fields $ \f -> do
      if all (`Set.member` valueRange f) (column c) then do
        putStrLn $ "  " ++ show (name f)
        return $ Just f
      else
        return Nothing
    return (c, catMaybes matchFields)
  
  let fieldOrder = columnsToFields columnPossibilities
      departureColumns = map fst $ filter (C8.isPrefixOf "departure" . name . snd) fieldOrder
      multiplied = product $ map (\c -> your !! c) departureColumns
  print multiplied
