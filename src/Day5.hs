module Day5 (main5) where

import IntCode (parse, evaluateUntilHaltWithInput)

type IT = [Integer]

solve1 :: IT -> [Integer]
solve1 = evaluateUntilHaltWithInput [1]

solve2 :: IT -> [Integer]
solve2 = evaluateUntilHaltWithInput [5]

main5 :: IO ()
main5 = do
  input <- parse <$> readFile "res/input5"
  print $ solve1 input
  print $ solve2 input

