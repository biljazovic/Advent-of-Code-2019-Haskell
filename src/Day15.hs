module Day15 (main15) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear.V2
import Control.Monad.RWS
import Control.Lens
import Debug.Trace (trace)
import Util (generateBlackAndWhiteImage, generateGraph)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Data.Maybe (fromJust)
import Codec.Picture (PixelRGB8(..))
import Data.Tuple (swap)

type IT = [Integer]

data ProgramState = PS {
  _outputStack :: [Int],
  _visited :: Map (V2 Int) Int
}

$(makeLenses ''ProgramState)

dirs :: [(Int, V2 Int)]
dirs = [(1, V2 0 1), (2, V2 0 (-1)), (3, V2 (-1) 0), (4, V2 1 0)]

oppositeTip :: Int -> Int
oppositeTip x = if x <= 2 then 3 - x else 7 - x

go :: RWS (V2 Int) [Int] ProgramState ()
go = forM_ dirs $ \(tip, dir) -> do
      tell [tip]
      out <- gets (head . _outputStack)
      curVisited <- gets _visited
      modify $ over outputStack tail
      curPos <- ask
      let newPos = curPos + dir
      if out == 0
         then return ()
         else do
           if Map.member newPos curVisited 
              then return ()
              else do
                modify $ over visited (Map.insert newPos out)
                local (+ dir) go
           tell [oppositeTip tip]
           modify $ over outputStack tail


generateMap :: IT -> Map (V2 Int) Int
generateMap xs = _visited stat where
  output = map fromIntegral $ fst $ evaluateUntilHaltWithInput (map fromIntegral input) xs
  (_, stat, input) = runRWS go (V2 0 0) (PS output (Map.singleton (V2 0 0) 1))

solve1 :: Map (V2 Int) Int -> Int
solve1 mapa = fromJust $ spLength (indexMap Map.! V2 0 0) (indexMap Map.! oxygen) graph where
  (graph, indexMap) = generateGraph mapa
  (oxygen, _) = Map.findMax $ Map.filter (== 2) mapa

solve2 :: Map (V2 Int) Int -> Int
solve2 mapa = (+ (-1)) . maximum $ map (length . unLPath) $ spTree (indexMap Map.! oxygen) graph where
  (graph, indexMap) = generateGraph mapa
  (oxygen, _) = Map.findMax $ Map.filter (== 2) mapa

drawPath :: Map (V2 Int) Int -> Map (V2 Int) Int
drawPath mapa = foldr (`Map.insert` 2) mapa coordPath where
  path = fromJust $ sp (indexMap Map.! V2 0 0) (indexMap Map.! oxygen) graph
  coordPath = map (reverseIndexMap Map.!) path
  reverseIndexMap = Map.fromList $ map swap $ Map.toList indexMap
  (graph, indexMap) = generateGraph mapa
  (oxygen, _) = Map.findMax $ Map.filter (== 2) mapa

toColor = \case
  0 -> PixelRGB8 0 0 0
  1 -> PixelRGB8 255 255 255
  2 -> PixelRGB8 255 0 0

main15 :: IO ()
main15 = do
  input <- parse <$> readFile "res/input15"
  let mapa = generateMap input
  generateBlackAndWhiteImage "res/output15.bmp" 0 (Map.insert (V2 0 0) 2 mapa) toColor
  generateBlackAndWhiteImage "res/output15path.bmp" 0 (drawPath mapa) toColor
  print $ solve1 mapa
  print $ solve2 mapa
