module Day10 (main10) where

import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2
import Data.Ord (Ordering(..), compare, comparing)
import Data.List (sortOn, sortBy, groupBy, maximumBy, delete, uncons)
import Data.Maybe (mapMaybe)

type IT = [V2 Int]

parse :: String -> IT
parse str = [V2 y x | (x, r) <- zip [0..] (lines str), (y, c) <- zip [0..] r, c == '#']

reduce :: Integral a => V2 a -> V2 a
reduce (V2 x y) = V2 (x `div` g) (y `div` g) where
  g = gcd x y

solve1 :: IT -> Int
solve1 input = maximum $ map (Set.size . f) input
  where 
    f x = Set.map (g x) (Set.fromList $ delete x input)
    g x y = reduce (y - x)

order :: V2 Int -> V2 Int -> V2 Int -> Ordering
order vc va vb
  | x1 >= 0 && x2 < 0 = LT
  | x1 < 0 && x2 >= 0 = GT
  | x1 == 0 && x2 == 0 = if (y1 > 0) == (y2 > 0)
                       then EQ
                       else compare y1 y2
  | otherwise = compare 0 (crossZ v1 v2) 
  where
    v1@(V2 x1 y1) = va - vc
    v2@(V2 x2 y2) = vb - vc

solve2 :: IT -> Int
solve2 input = 100 * solx + soly
  where
    (V2 solx soly) = go 199 trak
    go n trak'
      | n >= length trak' = go (n - length trak') $ map snd $ mapMaybe uncons trak'
      | otherwise = head (trak' !! n)
    trak = map (sortOn (sum . fmap abs . (centre -))) (f centre)
    centre = maximumBy (comparing (length . f)) input 
    f x = groupBy (\a b -> order x a b == EQ) $ sortBy (order x) (delete x input)

main10 :: IO ()
main10 = do
  input <- parse <$> readFile "res/input10"
  print $ solve1 input
  print $ solve2 input 
