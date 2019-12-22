module Day16 (main16) where

import Data.Char (digitToInt)
import Data.List (iterate', intersperse)

type IT = [Int]

parse :: String -> IT
parse = map digitToInt . head . lines

step :: [Int] -> [Int]
step xs = zipWith (curry f) xs [1..] where
  f (x, i) = (`mod` 10) . abs . sum $ zipWith (*) xs (tail $ concatMap (replicate i) $ cycle [0,1,0,-1])
  n = length xs

solve1 :: IT -> [Int]
solve1 initial = take 8 finalList where
  finalList = iterate' step initial !! 100

solve2 :: IT -> [Int]
solve2 initial = take 8 finalList where
  finalList = iterate' (scanr1 (\x y -> (x + y) `mod` 10)) initList !! 100
  initList = drop n $ concat $ replicate 10000 initial
  n = foldl (\s d -> s * 10 + d) 0 $ take 7 initial

main16 :: IO ()
main16 = do
  input <- parse <$> readFile "res/input16"
  print $ solve1 input
  print $ solve2 input 
