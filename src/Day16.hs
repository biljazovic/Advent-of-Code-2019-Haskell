{-# LANGUAGE BangPatterns #-}
module Day16 (main16) where

import Data.Char (digitToInt)
import Data.Matrix (toList, getCol, colVector, nrows, ncols, fromLists, Matrix(..), identity)
import Data.Vector (fromList, Vector(..))
import Data.List (iterate', intersperse)

type IT = [Int]

parse :: String -> IT
parse = map digitToInt . head . lines

step :: Int -> Matrix Int
step n = fromLists $ map f [1..n] where
  f x = take n . tail . concatMap (replicate x) $ cycle [0,1,0,-1]

modProduct :: Integral a => a -> Matrix a -> Matrix a -> Matrix a
modProduct p !m1 !m2 = fmap ((`mod` p) . abs) $! m1 * m2

solve1 :: IT -> [Int]
solve1 initial = take 8 $ toList finalVec where
  initialVec = fromList initial
  n = length initial
  finalVec = go 100 (colVector initialVec)
  stepn = step n
  go !x !v 
    | x == 0 = v
    | otherwise = go (x-1) $! modProduct 10 stepn v

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
