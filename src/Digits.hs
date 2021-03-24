{-# language TupleSections #-}

module Digits where

import Paths_metronome_cli
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

type Digits = [(Int, String)]

loadDigits :: IO Digits
loadDigits =
  let loadDigit = \i -> fmap (i,) . readFile =<< getDataFileName ("digits/" <> show i)
   in traverse loadDigit [0 .. 9]

digitsFor :: Int -> [Int]
digitsFor = map digitToInt . show

render :: Digits -> Int -> [String]
render digits num = 
  let thisDigits = fromJust . flip lookup digits <$> digitsFor num
   in thisDigits
