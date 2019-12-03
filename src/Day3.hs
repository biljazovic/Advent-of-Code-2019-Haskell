module Day3 (main3) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromJust)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Path = [(Char, Int)]

type Parser = Parsec Void String

parser :: Parser Path
parser = sepBy p (char ',') where
  p = do
    dir <- anySingle
    steps <- L.decimal
    pure (dir, steps)

data Coord = C { getX :: Int, getY :: Int, getSteps :: Int }
  deriving (Show)

manhattan :: Coord -> Int
manhattan (C x y _) = abs x + abs y

instance Eq Coord where
  C x1 y1 _ == C x2 y2 _ = (x1, y1) == (x2, y2)

instance Ord Coord where
  c1@(C x1 y1 _) <= c2@(C x2 y2 _)
    | m1 /= m2 = m1 < m2
    | otherwise = (x1, y1) <= (x2, y2)
      where
        m1 = manhattan c1
        m2 = manhattan c2

move :: Char -> Coord -> Coord
move c = \(C x y s)  -> C (x + dx) (y + dy) (s + 1) where
  (dx, dy) = case c of
               'R' -> (1, 0)
               'L' -> (-1, 0)
               'U' -> (0, 1)
               'D' -> (0, -1)

pathToCoords :: Path -> Set Coord
pathToCoords = snd . foldl f (C 0 0 0, S.empty) where
  f (coord, set) (dir, steps) = (coords !! steps, S.union set newCoords) where
    newCoords = S.fromList $ take steps $ tail coords
    coords = iterate (move dir) coord

solve1 :: [Path] -> Int
solve1 = manhattan . S.findMin . foldr1 S.intersection . map pathToCoords

pathToStepMap :: Path -> Map (Int, Int) Int
pathToStepMap = M.fromList . map (\(C x y s) -> ((x, y), s)) . S.elems . pathToCoords

solve2 :: [Path] -> Int
solve2 = minimum . M.elems . foldr1 (M.intersectionWith (+)) . map pathToStepMap

main3 :: IO ()
main3 = do
  input <- map (fromJust . parseMaybe parser) . lines <$> readFile "res/input3"
  print $ solve1 input
  print $ solve2 input 

