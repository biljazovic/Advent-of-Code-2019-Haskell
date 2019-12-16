module Day11 (main11) where

import IntCode (parse , evaluateUntilHaltWithInput)
import Data.Ord (comparing)
import Data.List (maximumBy, permutations, scanl)
import Control.Monad (replicateM)
import Linear.V2
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.List.Split (chunksOf)
import Util (generateBlackAndWhiteImage)
import Codec.Picture (PixelRGB8(..))

type IT = [Integer]

type RobotState = (V2 Int, V2 Int, Map (V2 Int) Int)

solve :: Integer -> IT -> Map (V2 Int) Integer
solve startingColor xs = finalMap where
  output = fst $ evaluateUntilHaltWithInput input xs
  input = map (fromIntegral . \(pos, _, mapa) -> Map.findWithDefault 0 pos mapa) states
  (_, _, finalMap) = last states
  states = scanl f (V2 0 0, V2 0 (-1), Map.singleton (V2 0 0) startingColor) $ chunksOf 2 output
  f (pos, dir, mapa) [color, turn] = (pos + newDir, newDir, newMapa) where
    newMapa = Map.insert pos color mapa
    newDir = case turn of
               1 -> nxt dir
               0 -> nxt $ nxt $ nxt dir
    nxt = \case
      V2 0 1 -> V2 (-1) 0
      V2 (-1) 0 -> V2 0 (-1)
      V2 0 (-1) -> V2 1 0
      V2 1 0 -> V2 0 1

solve1 :: IT -> Int
solve1 = Map.size . solve 0

solve2 :: IT -> Map (V2 Int) Integer
solve2 = solve 1

toColor x = PixelRGB8 y y y where
  y = fromIntegral x * 255

main11 :: IO ()
main11 = do
  input <- parse <$> readFile "res/input11"
  print $ solve1 input
  generateBlackAndWhiteImage "res/output11.bmp" 0 (solve2 input) toColor
