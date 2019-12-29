module Day24 (main24) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Util (generateMap, susedi)
import Linear.V2
import Linear.V3

solve1 :: Map (V2 Int) Char -> Int
solve1 grid = sum . map snd . filter ((== '#') . fst) $ zip (Map.elems (Map.mapKeys (\(V2 x y) -> V2 y x) found)) (iterate (*2) 1)
  where
    Just (found, _) = find (\(grid', seen) -> Set.member grid' seen) (zip grids gridsSet)
    gridsSet = scanl (flip Set.insert) Set.empty grids
    grids = iterate (\grid' -> Map.mapWithKey (transform susedi grid') grid') grid

transform :: (Ord k) => (k -> [k]) -> Map k Char -> k -> Char -> Char
transform susedi' grid coord c = let bugs = length $ filter (== '#') $ map (flip (Map.findWithDefault '.') grid) (susedi' coord)
                   in if | c == '#' && bugs /= 1 -> '.'
                         | c == '.' && bugs `elem` [1,2] -> '#'
                         | otherwise -> c

susedi3 :: V3 Int -> [V3 Int]
susedi3 (V3 x y z) = concatMap f (susedi (V2 x y))
  where
    f (V2 x' y')
      | x' == 2 && y' == 2 = case (x, y) of
                               (3, 2) -> [V3 4 i (z+1) | i <- [0..4]]
                               (2, 1) -> [V3 i 0 (z+1) | i <- [0..4]]
                               (2, 3) -> [V3 i 4 (z+1) | i <- [0..4]]
                               (1, 2) -> [V3 0 i (z+1) | i <- [0..4]]
      | x' == 5 = [V3 3 2 (z-1)]
      | x' == (-1) = [V3 1 2 (z-1)]
      | y' == 5 = [V3 2 3 (z-1)]
      | y' == (-1) = [V3 2 1 (z-1)]
      | otherwise = [V3 x' y' z]

solve2 :: Map (V2 Int) Char -> Int
solve2 grid = length $ filter (== '#') $ Map.elems ((iterate f initGrid) !! 200)
  where
    initGrid = Map.mapKeys (\(V2 x y) -> V3 x y 0) $ Map.delete (V2 2 2) grid
    f grid' = let grid'' = grid' `Map.union` (Map.fromList $ zip (concatMap susedi3 (Map.keys grid')) (repeat '.'))
               in Map.mapWithKey (transform susedi3 grid'') grid''

main24 = do
  input <- generateMap <$> readFile "res/input24"
  print $ solve1 input
  print $ solve2 input
