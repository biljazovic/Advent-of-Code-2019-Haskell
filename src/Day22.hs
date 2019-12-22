{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Day22 (main22) where

import Data.List.Split (splitOn)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Debug.Trace
import Data.Modular

data Shuffle = NewStack | Increment Int | Cut Int
  deriving (Show, Eq)

parse :: String -> [Shuffle]
parse = map f . lines
  where
    f (words -> strs) = case take 2 strs of
                          ["deal", "into"] -> NewStack
                          ["deal", "with"] -> Increment (read (strs !! 3))
                          ["cut", _] -> Cut (read (strs !! 1))

-- TODO Rewrite Part 1 with Part 2 logic
inverse :: Integer -> Integer -> Integer 
inverse q 1 = 1
inverse q p = (n * q + 1) `div` p
  where 
    n = p - inverse p (q `mod` p)

shuffle :: Shuffle -> Seq Int -> Seq Int
shuffle = \case
  NewStack -> Seq.reverse
  Cut n -> \s -> let m = if n >= 0
                            then n
                            else Seq.length s + n
                  in Seq.drop m s Seq.>< Seq.take m s
  Increment n -> \s -> let m = Seq.length s
                           inv = inverse (fromIntegral m) (fromIntegral n)
                        in Seq.fromFunction m (Seq.index s . (`mod` m) . (* (fromIntegral inv)))

solve1 :: [Shuffle] -> Int
solve1 shuffles = fromJust $ Seq.elemIndexL 2019 (foldl (flip shuffle) (Seq.iterateN 10007 (+1) 0) shuffles)

size = 119315717514047
times = 101741582076661

type M = Mod Integer 119315717514047

data LinPoly = LP M M
  deriving (Show, Eq)

shufflePosLP :: Shuffle -> LinPoly -> LinPoly
shufflePosLP shuf (LP a b) = case shuf of
  NewStack -> LP (-a) (-b-1)
  Cut (fromIntegral -> x) -> LP a (b + x)
  Increment (fromIntegral -> x) -> LP (a `div` x) (b `div` x)

eval :: LinPoly -> M -> M
eval (LP a b) x = a * x + b

solve2 :: [Shuffle] -> M
solve2 shuffles = a^times * 2020 + b * (a^times - 1) `div` (a-1)
  where
    LP a b = foldr shufflePosLP (LP 1 0) shuffles

main22 = do
  input <- parse <$> readFile "res/input22"
  print $ solve1 input
  print $ solve2 input
