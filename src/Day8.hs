module Day8 (main8) where

import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.List (minimumBy, find, transpose)
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)
import Data.Vector.Storable (fromList)
import Codec.Picture

type IT = String

parse :: String -> IT
parse = head . lines

(w, h) = (25, 6)

solve1 :: IT -> Int
solve1 = liftA2 (*) (count '1') (count '2') . minimumBy (comparing (count '0')) . chunksOf (w * h) where
  count n = length . filter (== n)

solve2 :: IT -> [Int]
solve2 = map (f . fromMaybe '2' . find (/= '2')) . transpose . chunksOf (w * h) where
  f = \case
    '0' -> 0
    '1' -> 255
    '2' -> 128

main8 :: IO ()
main8 = do
  input <- parse <$> readFile "res/input8"
  print $ solve1 input
  let arr = map fromIntegral $ solve2 input
  print (length arr)
  saveBmpImage "res/output8.bmp" $ ImageY8 $ Image w h $ fromList arr
