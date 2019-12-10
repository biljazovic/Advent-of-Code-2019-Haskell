module Day10 (main10) where

import Data.Set (Set)
import qualified Data.Set as Set

type IT = Set (Int, Int)

parse :: String -> IT
parse str = Set.fromList [(x, y) |  (x, r) <- zip [0..] (lines str),(y, c) <- zip [0..] r,  c == '#']

reduce :: 

solve1 :: IT -> Int
solve1 inputSet = Set.findMax $ map (Set.size . f) $ toList input 
  where 
    f x = Set.map (g x) inputSet ; g x y = reduce (y - x)

solve2 :: IT -> Int
solve2 = undefined

main10 :: IO ()
main10 = do
  input <- parse <$> readFile "res/input10"
  print $ solve1 input
  print $ solve2 input 
