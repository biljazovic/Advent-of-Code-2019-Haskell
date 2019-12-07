{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module IntCode where

import Data.Sequence (Seq, update, index, fromList)
import Data.List.Split (splitOn)
import Data.Either (either)
import Control.Lens hiding (element, index)
import Control.Lens.TH
import Control.Monad.State

parse :: String -> [Int]
parse = map read . splitOn ","

data XD = XD { _board :: Seq Int, _pos :: Int, _input :: [Int] }
data Status = Out Int | Continue | Halt

$(makeLenses ''XD)

fromLeft (Left a) = a

stepInstr :: Int -> [Either Int Int] -> State XD Status
stepInstr opcode (a:b:c:_) = do
  board' <- _board <$> get 
  let eval = either (board' `index`) id 
  let binInstr f = do
        modify $ over board (update (fromLeft c) (f (eval a) (eval b)))
        modify $ over pos (+3)
        return Continue
  let jump f = do
        modify $ over pos (if f (eval a) then const (eval b) else (+2))
        return Continue
  let cmp f = do
        modify $ over board (update (fromLeft c) (if f (eval a) (eval b) then 1 else 0))
        modify $ over pos (+3)
        return Continue
  case opcode of 
    1 -> binInstr (+)
    2 -> binInstr (*)
    3 -> do
      inputValue <- (head . _input) <$> get
      modify $ over board (update (fromLeft a) inputValue)
      modify $ over input tail
      modify $ over pos (+1)
      return Continue
    4 -> do
      modify $ over pos (+1)
      return (Out (eval a))
    5 -> jump (/= 0)
    6 -> jump (== 0)
    7 -> cmp (<)
    8 -> cmp (==)
    99 -> return Halt

evaluate :: State XD Status
evaluate = do
  board' <- _board <$> get
  pos' <- _pos <$> get
  let opcode = board' `index` pos'
      instrCode = opcode `mod` 100
      paramModes = map (`mod` 10) (iterate (`div` 10) (opcode `div` 100))
      args = zipWith f (map (index board') [(pos'+1)..]) paramModes
      f val = \case
        0 -> Left val
        1 -> Right val
  modify $ over pos (+1)
  stepInstr instrCode args

evaluateUntilHaltWithInput :: [Int] -> [Int] -> ([Int], XD)
evaluateUntilHaltWithInput inStack xs = runState go xd0 where
  go = do
    status <- evaluate
    case status of
      Halt -> return []
      Out x -> do
        xs <- go
        return (x : xs)
      Continue -> go
  xd0 = XD (fromList xs) 0 inStack
