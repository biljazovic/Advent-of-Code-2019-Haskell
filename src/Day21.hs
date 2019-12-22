module Day21 (main21) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Util (generateBlackAndWhiteImage)
import Data.Char

type IT = [Integer]

program1 :: String
program1 = unlines
  [
    "NOT J J",
    "AND A J",
    "AND B J",
    "AND C J",
    "NOT J J",
    "AND D J",
    "WALK"
  ]

program2 :: String
program2 = unlines
  [
    "NOT J J",
    "AND A J",
    "AND B J",
    "AND C J",
    "NOT J J",
    "AND D J",
    "OR H T",
    "OR E T",
    "AND T J",
    "RUN"
  ]

solve :: String -> IT -> Integer
solve program xs = last output
  where
    output = fst $ evaluateUntilHaltWithInput input xs
    input = map (fromIntegral . ord) program

main21 :: IO ()
main21 = do
  input <- parse <$> readFile "res/input21"
  print $ solve program1 input
  print $ solve program2 input
