module Day14 (main14) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

type IT = [([(String, Integer)], (String, Integer))]

parse :: String -> IT
parse = map f . lines where
  f str = (map parse1 (splitOn ", " ings), parse1 res) where
    (ings : res : _) = splitOn " => " str
    parse1 str1 = let res = splitOn " " str1 in (res !! 1, read (res !! 0)) 

solve :: Integer -> IT -> Integer
solve fuel svi = snd $ need ("FUEL", fuel) (Map.empty, 0) where
  mapa = Map.fromList $ map (\(ings, (resS, resI)) -> (resS, (ings, resI))) svi
  need (tko, kol) (waste, ore) 
    | tko == "ORE" = (waste, ore + kol)
    | otherwise = if cur >= kol
                  then (Map.adjust (+ (-kol)) tko waste, ore)
                  else let (ings, aval) = mapa Map.! tko
                           (times', ost') = (kol - cur) `divMod` aval
                           (times, ost) = if ost' == 0
                                          then (times', 0) 
                                          else (times' + 1, aval - ost')
                        in foldr (need . fmap (* times)) (Map.insert tko ost waste, ore) ings
      where
        cur = Map.findWithDefault 0 tko waste

solve1 :: IT -> Integer
solve1 = solve 1

-- return (Just x) if x is the first integer in [low,high] that satisfies f, 
-- or Nothing if none satisfy it (f must be increasing on [low,high]
binarySearch :: (Integer -> Bool) -> Integer -> Integer -> Maybe Integer
binarySearch f low high
  | low > high = Nothing
  | low == high = Just low
  | otherwise = let mid = (low + high) `div` 2
                in if f mid
                   then binarySearch f low mid
                   else binarySearch f (mid+1) high

solve2 :: IT -> Maybe Integer
solve2 svi = fmap (+ (-1)) $ binarySearch ((> 1000000000000) . flip solve svi) 1 1000000000000

main14 :: IO ()
main14 = do
  input <- parse <$> readFile "res/input14"
  print $ solve1 input
  print $ solve2 input 
