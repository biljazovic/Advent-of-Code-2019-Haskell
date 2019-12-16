{-# LANGUAGE BangPatterns #-}
module Day16 (main16) where

import Data.Char (digitToInt)
import Data.Matrix (toList, getCol, colVector, nrows, ncols, fromLists, Matrix(..), identity)
import Data.Vector (fromList, Vector(..))
import Data.List (iterate')

type IT = [Int]

parse :: String -> IT
parse = map digitToInt . head . lines

step :: Int -> Matrix Int
step n = fromLists $ map f [1..n] where
  f x = take n . tail . concatMap (replicate x) $ cycle [0,1,0,-1]

modProduct :: Integral a => a -> Matrix a -> Matrix a -> Matrix a
modProduct p !m1 !m2 = fmap ((`mod` p) . abs) $! m1 * m2

modExp :: Integral a => a -> a -> Matrix a -> Matrix a
modExp p x m0
  | x == 0 = identity (nrows m0)
  | otherwise = let (y, ost) = x `divMod` 2
                    m1 = modExp p y m0
                    m2 = modProduct p m1 m1
                in if ost == 0 then m2 else modProduct p m2 m0

--solve1 :: IT -> Vector Int
solve1 initial = toList finalVec where
  initialVec = fromList initial
  n = length initial
  finalVec = go 1 (colVector initialVec)
  stepn = step n
  go !x !v 
    | x == 0 = v
    | otherwise = go (x-1) $! modProduct 10 stepn v
    -- | otherwise = modProduct 10 stepn $ go (x-1) v
  --finalVec = iterate (modProduct 10 (stepn)) (colVector initialVec) !! 100
  --finalVec = modProduct 10 (modExp 10 1 (step n)) (colVector vec)
  --finalVec = modProduct 10 ((step n) ^ 100) (colVector vec)
  

solve2 :: IT -> Int
solve2 = undefined

main16 :: IO ()
main16 = do
  input <- parse <$> readFile "res/input16"
  print $ solve1 input
  --print $ solve2 input 
