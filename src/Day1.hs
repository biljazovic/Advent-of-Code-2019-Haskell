module Day1 (main1) where

type IT = [Int]

parse :: String -> IT
parse = map read . words

f :: Int -> Int
f x = x `div` 3 - 2

solve1 :: IT -> Int
solve1 = sum . map f

solve2 :: IT -> Int
solve2 = sum . concatMap (takeWhile (> 0) . drop 1 . iterate f)

main1 :: IO ()
main1 = do
  input <- parse <$> readFile "res/input1"
  print $ solve1 input
  print $ solve2 input 
