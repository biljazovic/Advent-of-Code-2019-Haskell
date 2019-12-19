{-# LANGUAGE ViewPatterns #-}

module Day18 (main18) where

import Util (generateMap, findInMap, susedi)
import Linear.V2
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe 
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

type IT = Map (V2 Int) Char

fullMask :: Int
fullMask = 2^26 - 1

type BfsState = (V2 Int, Int)

genericBfs :: Ord a => (a -> Bool)       -- goal
                    -> (a -> [a])        -- neighbors
                    -> a                 -- start
                    -> Maybe Int         -- distance from start to goal
genericBfs goal neighs start = bfs (Set.singleton start) (Seq.singleton (start, 0))
  where
    bfs seen queue = case queue of
      (Seq.viewl -> (cur, dist) Seq.:< rest) -> if goal cur
                                                   then Just dist
                                                   else let (newSeen, newQueue) = foldr g (seen, rest) (neighs cur)
                                                            g neigh (seen', queue') = if Set.member neigh seen'
                                                                                        then (seen', queue')
                                                                                        else (Set.insert neigh seen', queue' Seq.|> (neigh, dist+1))
                                                         in bfs newSeen newQueue
      _                                      -> Nothing

f grid mask sused = do
    c <- grid Map.!? sused
    guard (c /= '#')
    let ci = ord (toLower c) - ord 'a'
    guard (c == '.' || isLower c || testBit mask ci)
    return $ if isLower c then setBit mask ci else mask

solve1 :: IT -> Maybe Int
solve1 grid = genericBfs ((== fullMask) . snd) neighs (initPos, 0)
  where
    initPos = findInMap ['@'] grid
    neighs (pos, mask) = mapMaybe (\sused -> fmap (sused,) (f grid mask sused)) $ susedi pos

solve2 :: IT -> Maybe Int
solve2 grid = genericBfs ((== fullMask) . snd) neighs (initPoss, 0)
  where
    initPoss = map (+ atPos) [V2 1 1, V2 (-1) 1, V2 (-1) (-1), V2 1 (-1)]
    atPos = findInMap ['@'] grid
    gridM = Map.fromList (map (, '#') (susedi atPos)) `Map.union` grid
    neighs (poss, mask) = let g n = mapMaybe (\sused -> fmap (\newMask -> (take n poss ++ [sused] ++ drop (n+1) poss, newMask)) (f gridM mask sused)) (susedi (poss !! n))
                           in concatMap g [0..3]

main18 = do
  grid <- generateMap <$> readFile "res/input18"
  print $ solve1 grid
  print $ solve2 grid
