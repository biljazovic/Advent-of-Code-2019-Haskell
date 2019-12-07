{-# LANGUAGE LambdaCase #-}
module IntCode (parse, evaluateUntilHaltWithInput, evaluate, XD) where

import Data.Sequence (Seq, update, index, fromList)
import Data.List.Split (splitOn)
import Data.Either (either)

parse :: String -> [Int]
parse = map read . splitOn ","

type XD = (Seq Int, Int, [Int], [Int]) -- (board, position, input stack, output stack)

fromLeft (Left a) = a

stepInstr :: Int -> [Either Int Int] -> XD -> Maybe XD
stepInstr opcode (a:b:c:_) xd@(board, pos, inStack, outStack) =
  case opcode of 
    1 -> binInstr (+)
    2 -> binInstr (*)
    3 -> Just (update (fromLeft a) (head inStack) board, pos + 1, tail inStack, [])
    4 -> Just (board, pos + 1, inStack, [eval a])
    5 -> jump (/= 0)
    6 -> jump (== 0)
    7 -> cmp (<)
    8 -> cmp (==)
    99 -> Nothing
    where
      binInstr f = Just (update (fromLeft c) (f (eval a) (eval b)) board, pos + 3, inStack, [])
      jump f = Just (board, if f (eval a) then eval b else pos + 2, inStack, [])
      cmp f = Just (update (fromLeft c) (if f (eval a) (eval b) then 1 else 0) board, pos + 3, inStack, [])
      eval = either (board `index`) id 
  

evaluate :: XD -> Maybe XD
evaluate xd@(board, pos, inStack, outStack) = stepInstr instrCode args (board, pos+1, inStack, outStack) where
  opcode = board `index` pos
  instrCode = opcode `mod` 100
  paramModes = map (`mod` 10) (iterate (`div` 10) (opcode `div` 100))
  args = zipWith f (map (index board) [(pos+1)..]) paramModes
  f val = \case
    0 -> Left val
    1 -> Right val

evaluateUntilHaltWithInput :: [Int] -> [Int] -> [Int]
evaluateUntilHaltWithInput inStack xs = go xd0 where
  go xd = case evaluate xd of 
            Just xd1@(_, _, _, outTemp) -> outTemp ++ go xd1
            Nothing -> []
  xd0 = (fromList xs, 0, inStack, [])

