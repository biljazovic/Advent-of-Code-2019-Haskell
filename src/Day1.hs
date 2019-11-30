module Day1 (main1) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Debug.Trace
import Control.Applicative (liftA2)
import Data.Ord

type Pair = (Int, Int)

type IT = [Int]

parse :: String -> IT
parse = map read . words

solve1 :: IT -> String
solve1 xs = unwords $ map show sorted where
  sorted = sortBy (comparing Down) xs

solve2 :: IT -> Int
solve2 = undefined

main1 :: IO ()
main1 = do
  input <- parse <$> readFile "res/input1"
  print $ solve1 input
  --print $ solve2 input 
