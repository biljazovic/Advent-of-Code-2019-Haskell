module Day7 (main7) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Data.Ord (comparing)
import Data.List (maximumBy, permutations)
import Control.Monad (replicateM)

type IT = [Int]

solve1 :: IT -> Int
solve1 xs = maximum $ do
  params <- permutations [0..4]
  let outStack = foldr f [0] params where
      f param prevOut = fst $ evaluateUntilHaltWithInput (param : prevOut) xs
  return $ head outStack

solve2 :: IT -> Int
solve2 xs = maximum $ do
  [pA, pB, pC, pD, pE] <- permutations [5..9]
  let outE = fst $ evaluateUntilHaltWithInput (pE : outD) xs
      outD = fst $ evaluateUntilHaltWithInput (pD : outC) xs
      outC = fst $ evaluateUntilHaltWithInput (pC : outB) xs
      outB = fst $ evaluateUntilHaltWithInput (pB : outA) xs
      outA = fst $ evaluateUntilHaltWithInput (pA : 0 : outE) xs
  return $ head $ reverse outE

main7 :: IO ()
main7 = do
  input <- parse <$> readFile "res/input7"
  print $ solve1 input
  print $ solve2 input 

