module Day17 (main17) where

import IntCode (parse, evaluateUntilHaltWithInput)
import Util (susedi)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (chr, ord)
import Linear.V2
import Control.Monad (forM_, guard)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<|>))
import Data.List (lookup, group, intersperse)

type IT = [Integer]

generateMap :: IT -> Map (V2 Int) Char
generateMap xs = grid where
  output = map (chr . fromIntegral) . fst $ evaluateUntilHaltWithInput [] xs
  grid = Map.fromList . concat . zipWith (\j ics -> map (\(i, c) -> (V2 i j, c)) ics) [0..] . map (zip [0..]) $ lines output

solve1 :: Map (V2 Int) Char -> Int
solve1 grid = sum . map product $ filter isCross (Map.keys grid) where
  isCross coord = isJust $ forM_ (coord : susedi coord) $ \sused -> do
    c <- grid Map.!? sused
    guard (c == '#')

dirs = [('>', V2 1 0), ('<', V2 (-1) 0), ('v', V2 0 1), ('^', V2 0 (-1))]

solve2 grid = groupedCode where
  groupedCode = concat $ map g $ group code
  g cs = if length cs == 1 then cs else show $ length cs
  code = fromJust $ go initPos (fromJust $ lookup (grid Map.! initPos) dirs)
  initPos = fst . head . filter ((`elem` (map fst dirs)) . snd) . Map.toList $ grid
  right (V2 x y) = V2 (-y) x
  left = right . right . right
  go pos dir = let f dir' = do
                        c <- Map.lookup (pos + dir') grid
                        guard (c == '#')
                        go (pos + dir') dir'
               in do
                 (('1':) <$> f dir) 
                    <|> ((['L','1']++) <$> f (left dir))
                    <|> ((['R','1']++) <$> f (right dir)) 
                    <|> Just []

main17 :: IO ()
main17 = do
  input <- parse <$> readFile "res/input17"
  let grid = generateMap input
  print $ solve1 grid -- part 1 solution
  print $ solve2 grid -- only generates movement instructions
  sol <- map (fromIntegral . ord) <$> readFile "res/output17" -- generated manually with vim
  print $ last . fst $ evaluateUntilHaltWithInput sol (2 : tail input) -- part 2 solution
