module Day6 (main6) where

import Data.Maybe (fromJust, maybe)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type IT = Map String String

parse :: String -> Maybe IT
parse = fmap Map.fromList . mapM f . words where
  f s = case splitOn ")" s of
          [s1, s2] -> Just (s2, s1)
          _ -> Nothing

solve1 :: IT -> Int
solve1 sun = sum calcs where
  calc y = maybe 1 (+ 1) (calcs Map.!? y)
  calcs = fmap calc sun

solve2 :: String -> String -> IT -> Int
solve2 x1 x2 sun = length p1 + length p2 - 2 * k - 2 where
  p1 = path x1
  p2 = path x2
  k = Set.size $ Set.intersection (Set.fromList p1) (Set.fromList p2)
  path x = x : maybe [] path (sun Map.!? x)

main6 :: IO ()
main6 = do
  input <- fromJust . parse <$> readFile "res/input6"
  print $ solve1 input
  print $ solve2 "YOU" "SAN" input 

