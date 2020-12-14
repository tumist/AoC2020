{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import qualified Data.Map as Map
import Data.Map ( Map )

import Data.Bits
import Data.Maybe ( mapMaybe )
import Data.Word
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 ( ByteString )

data Instruction
  = Mask ByteString
  | Mem Word64 Word64
  deriving Show

parseInstructions :: Parser [Instruction]
parseInstructions = many' $ do
  instr <- parseInstruction
  choice [endOfLine, endOfInput]
  pure instr
  where
    parseInstruction = choice [parseMask, parseMem]
    parseMask = do
      string "mask" >> skipSpace >> char '=' >> skipSpace
      Mask <$> AP.take 36
    parseMem = do
      string "mem" >> char '['
      address <- decimal
      char ']' >> skipSpace >> char '=' >> skipSpace
      Mem address <$> decimal

getInput :: FilePath -> IO (Either String [Instruction])
getInput = fmap (parseOnly parseInstructions) . BS.readFile

data State
  = State { mask   :: Maybe ByteString
          , memory :: Map Word64 Word64
          , inserts :: Int }
  deriving Show

initialState :: State
initialState = State Nothing Map.empty 0

apply :: State -> Instruction -> State
apply st (Mask bs) = st { mask = Just bs }
apply st@State{ mask = Just mask } (Mem adr val) = 
  st { memory = Map.insert adr (maskFunc mask val) (memory st) }
apply State{ mask = Nothing } (Mem _ _) =
  error "Attempting to insert value without set mask"

applyFloating :: State -> Instruction -> State
applyFloating st@State{ mask = Just mask, inserts = inserts } (Mem adr val) =
  let addresses = floating mask adr
      aLen = length addresses
  in st { memory = foldl (\m a -> Map.insert a val m) (memory st) addresses 
        , inserts = inserts + aLen }
applyFloating st (Mask bs) = st { mask = Just bs }

floating :: ByteString -> Word64 -> [Word64]
floating mask = go 0 (C8.unpack . BS.reverse $ mask)
  where
    go :: Int -> [Char] -> Word64 -> [Word64]
    go _ [] a = return a
    go ix ('0':r) a = go (ix+1) r a
    go ix ('1':r) a = go (ix+1) r (setBit a ix)
    go ix ('X':r) a = [setBit a ix, clearBit a ix] >>= go (ix+1) r

-- Generate a mask function from its ByteString representation
maskFunc :: ByteString -> (Word64 -> Word64)
maskFunc bs = foldl1 (.) $ mapMaybe bits idxs
  where
    idxs = C8.zip (BS.pack [0..]) (BS.reverse bs)
    bits (idxChr, chr) = 
      let idx = fromEnum idxChr
      in case chr of
        '1' -> Just $ flip setBit idx
        '0' -> Just $ flip clearBit idx
        'X' -> Nothing

sumMemory :: State -> Word64
sumMemory = sum . Map.elems . memory

main :: IO ()
main = do
  Right instr <- getInput "input"
  -- Part 1
  --print $ sumMemory $ foldl apply initialState instr

  -- Part 2
  let endstate = foldl applyFloating initialState instr
  print $ sumMemory endstate
  -- looking for clashes
  --putStrLn $ "inserts: " ++ show (inserts endstate)
  --putStrLn $ "keys:    " ++ show (length (Map.keys (memory endstate)))

