module Day23 (main23) where

import IntCode (feed, expect, run, parse, evaluate, ExtStatus(..), XD(..))
import Util (generateBlackAndWhiteImage)
import Data.Char
import Data.List (mapAccumL, find, delete, nub, (\\))
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Control.Monad

type IT = [Integer]

step :: [ExtStatus] -> [(Integer, (Integer, Integer))] -> ([ExtStatus], [(Integer, (Integer, Integer))])
step st queue = (st1, queue1)
  where
    ((queue1, _), st1) = mapAccumL h (queue, 0) st
    h (queue', n) = (\(x, y) -> ((x, n+1), y)) . \case
      Halt xd -> (queue', Halt xd)
      outg@(Out _ _) -> let (addr:x:y:[], new) = expect 3 outg 
                         in ((addr, (x, y)) : queue', new)
      ing@(In _) -> let (queue'', xy) = case find ((== n) . fst) queue' of
                                         Nothing -> (queue', [-1])
                                         Just el@(_, (x, y)) -> (delete el queue', [x, y])
                     in (queue'', feed ing xy)

solve1 xs = find isJust $ map (\(_, _, z) -> z) nicss
  where
    nics = map f [0..49]
    f n = feed (run xs) [n]
    nicss = iterate go $ (nics, [], Nothing)
    go (st, queue, _) = let (st1, queue1) = step st queue
                         in (st1, queue1, find ((== 255) . fst) queue1)

waiting :: ExtStatus -> Bool
waiting (In _) = True
waiting _ = False

solve2 xs = head $ head $ filter (\sentys -> not (null (sentys \\ (nub sentys)))) $ map (\(_, _, _, z) -> z) nicss
  where
    nics = map f [0..49]
    f n = feed (run xs) [n]
    nicss = iterate go $ (nics, [], Nothing, [])
    go (st, queue, nat, sentys) = let (st1, queue1) = step st queue
                                      packet255 = find ((== 255) . fst) queue1
                                      nat1 = packet255 <|> nat
                                      queue2 = if isJust packet255 then delete (fromJust packet255) queue1 else queue1
                                      conts = do
                                        guard (all waiting st1 && null queue2)
                                        (_, (x, y)) <- nat1
                                        Just ([(0, (x, y))], y)
                                      queue3 = maybe queue2 (fst) conts
                                      sentys1 = maybe sentys (\(_, y) -> y : sentys) conts
                                   in (st1, queue3, nat1, sentys1)

main23 :: IO ()
main23 = do
  input <- parse <$> readFile "res/input23"
  print $ solve1 input
  print $ solve2 input
