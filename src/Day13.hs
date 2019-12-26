module Day13 (main13) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Data.List.Split (chunksOf)
import Data.Set (Set)
import qualified Data.Set as Set

type IT = [Integer]

solve1 :: IT -> Int
solve1 = length . filter ((== 2) . (!! 2)) . chunksOf 3 . evaluateUntilHaltWithInput []

data Stat = None | Paddle Integer | Ball Integer

solve2 :: IT -> Integer
solve2 xs = (!! 2) . last . filter (\[x,y,c] -> x == -1 && y == 0) $ output where
  output = chunksOf 3 $ evaluateUntilHaltWithInput input (2 : tail xs)
  input = go (None, output)
  go (stat, [x, y, c] : rest) = case c of
                                  4 -> case stat of
                                         Paddle px -> move px x : go (Paddle (px + move px x), rest)
                                         _ -> go (Ball x, rest)
                                  3 -> case stat of
                                         Ball bx -> move x bx : go (Paddle x, rest)
                                         _ -> go (Paddle x, rest)
                                  _ -> go (stat, rest)
  move paddlex ballx = case compare paddlex ballx of
                         EQ -> 0
                         LT -> 1
                         GT -> -1
                              

main13 :: IO ()
main13 = do
  input <- parse <$> readFile "res/input13"
  print $ solve1 input
  print $ solve2 input

