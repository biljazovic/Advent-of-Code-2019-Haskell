module Day4 (main4) where

import Data.List (group)
import Data.Ix (range)

type IT = (Int, Int)

parse :: String -> IT
parse = (\(x : y : _) -> (x, y)) . map read . words

solve :: (Int -> Bool) -> IT -> Int
solve valid = length . filter valid . range

solve1 :: IT -> Int
solve1 = solve valid where
  valid num = hasDoubles str && increasing str where
    str = show num
    hasDoubles = any ((> 1) . length) . group


solve2 :: IT -> Int
solve2 = solve valid where
  valid num = hasExactlyDouble str && increasing str where
    str = show num
    hasExactlyDouble = any ((== 2) . length) . group 

increasing :: String -> Bool
increasing str = (not . or) $ zipWith (>) str (tail str)

main4 :: IO ()
main4 = do
  input <- parse <$> readFile "res/input4"
  print $ solve1 input
  print $ solve2 input 

