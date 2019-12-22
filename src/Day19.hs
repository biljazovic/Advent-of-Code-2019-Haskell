module Day19 (main19) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2
import Util (generateBlackAndWhiteImage)
import Codec.Picture (PixelRGB8(..))
import Data.Ord (comparing)
import Data.List (minimumBy)

type IT = [Integer]

solve1 :: IT -> Int
solve1 xs = length . filter (== 1) $ output where
  output = concatMap (fst . flip evaluateUntilHaltWithInput xs) input
  input = [[x, y] | x <- [0..49], y <- [0..49]]

generateMap' :: IT -> Map (V2 Int) Int
generateMap' xs = grid 
  where
    grid = Map.fromList $ zip coords output
    output = map (fromIntegral . head . fst . flip evaluateUntilHaltWithInput xs) input
    coords = map (fmap fromIntegral . \[x,y] -> V2 x y) input
    input = [[x, y] | x <- [0..1200], y <- [0..1200], y `div` 2 < x && x < y `div` 14 * 11]

solve2 grid = minimumBy (comparing $ sum . fmap abs) . filter good $ cands
  where
    cands = [V2 x y | x <- [0..1200], y <- [0..1200]]
    good coord = all g [coord, coord + V2 99 0, coord + V2 0 99, coord + V2 99 99]
    g coord = Map.findWithDefault 0 coord grid == 1

toColor = \case
  0 -> PixelRGB8 0 0 0
  1 -> PixelRGB8 255 255 255

main19 :: IO ()
main19 = do
  input <- parse <$> readFile "res/input19"
  print $ solve1 input
  let mapa = generateMap' input
  generateBlackAndWhiteImage "res/output19.bmp" 0 mapa toColor
  print $ solve2 mapa
