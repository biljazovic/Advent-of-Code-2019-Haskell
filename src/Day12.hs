module Day12 (main12) where

import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Linear.V3
import Data.Void
import Control.Lens ((^.))
import Data.List (delete, zipWith3)
import Data.Set (Set)
import qualified Data.Set as Set

type Parser = Parsec Void String

data Moon = Moon {
  _position :: !(V3 Int),
  _velocity :: !(V3 Int)
} deriving (Show, Eq, Ord)

type IT = [Moon]

parser :: Parser (V3 Int)
parser = do
  x <- string "<x=" *> signedInteger
  y <- char ',' *> space *> string "y=" *> signedInteger
  z <- char ',' *> space *> string "z=" *> signedInteger
  string ">" *> space
  pure (V3 x y z)
    where
      signedInteger = L.signed space L.decimal

total :: Moon -> Int
total (Moon pos vel) = f pos * f vel where
  f = sum . fmap abs

step :: [(Int, Int)] -> [(Int, Int)]
step ps = map f ps where
  g = \case
    EQ -> 0
    LT -> 1
    GT -> -1
  f p@(pos, vel) = (pos + newVel, newVel) where
    newVel = vel + sum (map (g . uncurry compare . (pos,) . fst) $ delete p ps)

stepA :: IT -> IT
stepA moons = zipWith3 (\(px, vx) (py, vy) (pz, vz) -> Moon (V3 px py pz) (V3 vx vy vz)) (step' _x) (step' _y) (step' _z) where
  step' l = step $ zip (map ((^.l) . _position) moons) (map ((^.l) . _velocity) moons)

solve1 :: IT -> Int
solve1 moons = sum $ map total $ iterate stepA moons !! 1000

solve2 :: IT -> Int
solve2 moons = foldr1 lcm [steps' _x, steps' _y, steps' _z] where
  steps' l = steps $ zip (map ((^.l) . _position) moons) (map ((^.l) . _velocity) moons)
  steps ps = go ps Set.empty 
  go ps visited = if Set.member ps visited
                  then 0
                  else 1 + go (step ps) (Set.insert ps visited)

main12 :: IO ()
main12 = do
  input <- map (\pos -> Moon pos (V3 0 0 0)) . mapMaybe (parseMaybe parser) . lines <$> readFile "res/input12"
  print $ solve1 input
  print $ solve2 input 
