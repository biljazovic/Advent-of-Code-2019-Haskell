module Util (generateGraph, generateBlackAndWhiteImage, susedi) where

import Codec.Picture
import Control.Lens ((^.))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Linear.V2
import Data.Word
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

generateBlackAndWhiteImage :: String -> a -> Map (V2 Int) a -> (a -> PixelRGB8) -> IO ()
generateBlackAndWhiteImage filename defaultValue mapa toColor = saveBmpImage filename image where
  image = ImageRGB8 $ generateImage f (maxX-minX+1) (maxY-minY+1) 
  f x y = toColor $ Map.findWithDefault defaultValue (V2 (x+minX) (y+minY)) mapa
  coords = Map.keysSet mapa
  coordsx = Set.map (^. _x) coords
  coordsy = Set.map (^. _y) coords
  [minX, maxX] = map ($ coordsx) [Set.findMin, Set.findMax]
  [minY, maxY] = map ($ coordsy) [Set.findMin, Set.findMax]

susedi :: V2 Int -> [V2 Int]
susedi coord = map (+ coord) [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

generateGraph :: Map (V2 Int) Int -> (Gr Int Int, Map (V2 Int) Int)
generateGraph mapa = (mkGraph nodes edges, indexMap) where
  coords = Set.toList $ Map.keysSet mapa
  indexMap = Map.fromList $ zip coords [1..]
  nodes = map (\(coord, index) -> (index, mapa Map.! coord)) $ zip coords [1..]
  edges = concatMap f coords
  f coord = mapMaybe g (susedi coord) where
    g sused = do
      tip <- Map.lookup sused mapa
      guard (tip > 0)
      return (indexMap Map.! coord, indexMap Map.! sused, 1)
