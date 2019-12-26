module Day2 (main2) where

import Data.Sequence (Seq, update, index, fromList)
import Control.Monad (guard)
import IntCode (run, parse, _board, ExtStatus(..))
import Data.Foldable (toList)

type IT = [Integer]

evaluate :: Integer -> Integer -> IT -> Integer
evaluate noun verb xs = ((`index` 0) . _board) xdf where
  seq0 = update 2 verb (update 1 noun (fromList xs))
  Halt xdf = run (toList seq0)

solve1 :: IT -> Integer
solve1 = evaluate 12 2

solve2 :: IT -> Integer
solve2 xs = head $ do
  noun <- [0..99]
  verb <- [0..99]
  guard (evaluate noun verb xs == 19690720)
  pure (100 * noun + verb)

main2 :: IO ()
main2 = do
  input <- parse <$> readFile "res/input2"
  print $ solve1 input
  print $ solve2 input 

