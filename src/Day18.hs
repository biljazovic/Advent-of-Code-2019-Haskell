module Day18 (main18) where

import Util (generateMap, findInMap, susedi, genericBfs, findInArray)
import Linear.V2
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe 
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import Data.Array.Unboxed

type IT = UArray (V2 Int) Char

{-# INLINE f #-}
f :: Bool -> UArray (V2 Int) Char -> Int -> V2 Int -> Maybe Int
f skipGates grid mask sused = do
    let c = grid ! sused
    guard (c /= '#')
    let ci = ord (toLower c) - ord 'a'
    guard (c == '.' || (isUpper c && skipGates) || isLower c || (isUpper c && testBit mask ci))
    return $ if isLower c then setBit mask ci else mask

dfs :: IT -> V2 Int -> Int
dfs grid start = snd $ go start (Set.singleton start)
  where
    go :: V2 Int -> Set (V2 Int) -> (Set (V2 Int), Int)
    go pos seen = foldr g (seen, 0) (susedi pos)
    g sused (seen', mask) = let c = grid ! sused
                                newMask = if isLower c 
                                             then setBit mask1 (ord c - ord 'a')
                                             else mask1
                                (seen1, mask1) = go sused (Set.insert sused seen')
                             in if c == '#' || Set.member sused seen'
                                   then (seen', mask)
                                   else (seen1, newMask .|. mask)

solve1 :: IT -> Maybe Int
solve1 grid = genericBfs ((== fullMask) . snd) neighs (initPos, 0)
  where
    fullMask = dfs grid initPos
    initPos = findInArray ['@'] grid
    neighs (pos, mask) = mapMaybe (\sused -> fmap (sused,) (f False grid mask sused)) $ susedi pos

solve2 :: IT -> Int -- wrong solution, doesn't work on test inputs
solve2 grid = sum results 
  where
    results = mapMaybe solveOne initPoss
    solveOne pos = genericBfs ((== (dfsMap Map.! pos)) . snd) neighs (pos, 0)
    dfsMap = Map.fromList $ zip initPoss (map (dfs gridM) initPoss)
    neighs (pos, mask) = mapMaybe (\sused -> fmap (sused,) (f True gridM mask sused)) $ susedi pos
    initPoss = map (+ atPos) [V2 1 1, V2 1 (-1), V2 (-1 ) 1, V2 (-1) (-1)]
    atPos = findInArray ['@'] grid
    gridM = grid // map (, '#') (susedi atPos)

main18 = do
  grid <- generateMap <$> readFile "res/input18"
  let arrayGrid = array (fst (Map.findMin grid), fst (Map.findMax grid)) $ Map.toList grid :: IT
  print $ solve1 arrayGrid
  print $ solve2 arrayGrid
