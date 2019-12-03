module Day2 (main2) where

import Data.Sequence (Seq, update, index, fromList)
import Data.List (find)
import Data.Maybe (fromJust)
import Control.Monad (guard)

type Pair = (Int, Int)

type IT = [Int]

parse :: String -> IT
parse = map read . words

step :: (Seq Int, Int) -> (Seq Int, Int)
step (seq, pos) = if opcode == 99 then (seq, pos) else (seq', pos + 4) where
  opcode = seq `index` pos
  xp = seq `index` (pos + 1)
  yp = seq `index` (pos + 2)
  rp = seq `index` (pos + 3)
  x = seq `index` xp
  y = seq `index` yp
  r = case opcode of 
        1 -> x + y
        2 -> x * y
  seq' = update rp r seq

evaluate :: Int -> Int -> IT -> Int
evaluate noun verb xs = seqf `index` 0 where
  seq0 = update 2 verb (update 1 noun (fromList xs))
  (seqf, _) = fromJust $ find (\(seq, pos) -> seq `index` pos == 99) $ iterate step (seq0, 0)

solve1 :: IT -> Int
solve1 = evaluate 12 2

solve2 :: IT -> Int
solve2 xs = head $ do
  noun <- [0..99]
  verb <- [0..99]
  guard (evaluate noun verb xs == 19690720)
  pure (100 * noun + verb)

-- TODO make it work with commas in input and make it more efficient with arrays
main2 :: IO ()
main2 = do
  input <- parse <$> readFile "res/input2"
  print $ solve1 input
  print $ solve2 input 

