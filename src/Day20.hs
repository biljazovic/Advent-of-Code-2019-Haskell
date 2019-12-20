module Day20 (main20) where

import Util (generateMap, findInMap, susedi, genericBfs)
import Linear.V2
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe 
import Debug.Trace (trace)
import Data.Array.Unboxed
import Data.Ix (inRange)

type Board = UArray (V2 Int) Char

indexWithDefault :: Char -> Board -> V2 Int -> Char
indexWithDefault def arr j = if inRange (bounds arr) j
                                then arr ! j
                                else def

getPortal :: Board -> V2 Int -> Maybe String
getPortal grid pos = do
  guard (grid ! pos == '.')
  foldr1 mplus $ map f (susedi pos)
    where
      f sused = do
        let c = indexWithDefault '#' grid sused
        guard (isUpper c)
        let str = [c, grid ! (sused + (sused - pos))]
        return $ if sum (sused - pos) < 0 then reverse str else str

solve1 :: Board -> Maybe Int
solve1 grid = genericBfs goal neighs start
  where
    portals = Map.fromListWith (++) . mapMaybe (\coord -> fmap (,[coord]) (getPortal grid coord)) $ indices grid
    goal = (== goalPos)
    goalPos = head $ portals Map.! "ZZ"
    start = head $ portals Map.! "AA"
    neighs pos = filter ((== '.') . indexWithDefault '#' grid) (susedi pos) ++ fromMaybe [] (buddy pos)
    buddy pos = fmap (\portal -> filter (/= pos) $ portals Map.! portal) $ getPortal grid pos

isOuter :: (V2 Int, V2 Int) -> V2 Int -> Bool
isOuter ((V2 smalx smaly), (V2 bigx bigy)) (V2 posx posy) = any (< 5) [posx-smalx, bigx-posx, posy-smaly, bigy-posy]

solve2 :: Board -> Maybe Int
solve2 grid = genericBfs goal neighs (startPos, 0)
  where
    portals = Map.fromListWith (++) . mapMaybe (\coord -> fmap (,[coord]) (getPortal grid coord)) $ indices grid
    goal = (== (goalPos, 0))
    goalPos = head $ portals Map.! "ZZ"
    startPos = head $ portals Map.! "AA"
    neighs (pos, level) = (map (,level) $ filter ((== '.') . indexWithDefault '#' grid) (susedi pos)) ++ fromMaybe [] (buddy pos level)
    buddy pos level = do
      portal <- getPortal grid pos
      let newLevel = level + (if isOuter (bounds grid) pos then (-1) else 1) 
      guard (newLevel >= 0)
      otherSide <- listToMaybe $ filter (/= pos) $ portals Map.! portal
      return [(otherSide, newLevel)]

main20 = do
  grid <- generateMap <$> readFile "res/input20"
  let gridArray = array (fst (Map.findMin grid), fst (Map.findMax grid)) $ Map.toList grid :: Board
  print $ solve1 gridArray
  print $ solve2 gridArray
