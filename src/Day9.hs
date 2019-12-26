module Day9 (main9) where

import IntCode (parse, evaluateUntilHaltWithInput)

type IT = [Integer]

solve1 :: IT -> [Integer]
solve1 = evaluateUntilHaltWithInput [1]

solve2 :: IT -> [Integer]
solve2 = evaluateUntilHaltWithInput [2]

main9 :: IO ()
main9 = do
  input <- parse <$> readFile "res/input9"
  print $ solve1 input
  print $ solve2 input

