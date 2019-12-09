module IntCode where

import Data.Sequence (Seq, update, index, fromList, (!?))
import Data.List.Split (splitOn)
import Data.Either (either)
import Control.Lens hiding (element, index)
import Data.Maybe (fromMaybe)
import Control.Lens.TH
import Control.Monad.State

parse :: String -> [Integer]
parse = map read . splitOn ","

data XD = XD { _board :: Seq Integer, _pos :: Int, _relativeBase :: Int, _input :: [Integer] }
data Status = Out Integer | Continue | Halt

$(makeLenses ''XD)

fromLeft (Left a) = a

updateInf' :: a -> Int -> a -> Seq a -> Seq a
updateInf' def pos val seq0 = update pos val seq1 where
  seq1
    | pos >= length seq0 = seq0 <> fromList (replicate (2 * (pos - length seq0 + 1)) def)
    | otherwise = seq0

indexInf' :: a -> Seq a -> Int -> a
indexInf' def seq0 pos = fromMaybe def (seq0 !? pos)

updateInf = updateInf' 0
indexInf = indexInf' 0

stepInstr :: Integer -> [Either Int Integer] -> State XD Status
stepInstr opcode (a:b:c:_) = do
  board' <- gets _board 
  let eval = either (indexInf board') id 
  let binInstr f = do
        modify $ over board (updateInf (fromLeft c) (f (eval a) (eval b)))
        modify $ over pos (+3)
        return Continue
  let jump f = do
        modify $ over pos (if f (eval a) then const (fromIntegral $ eval b) else (+2))
        return Continue
  let cmp f = do
        modify $ over board (updateInf (fromLeft c) (if f (eval a) (eval b) then 1 else 0))
        modify $ over pos (+3)
        return Continue
  case opcode of 
    1 -> binInstr (+)
    2 -> binInstr (*)
    3 -> do
      inputValue <- gets (head . _input)
      modify $ over board (updateInf (fromLeft a) inputValue)
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
    9 -> do
      modify $ over relativeBase (+ (fromIntegral $ eval a))
      modify $ over pos (+1)
      return Continue
    99 -> return Halt

evaluate :: State XD Status
evaluate = do
  board' <- gets _board
  pos' <- gets _pos
  relativeBase' <- gets _relativeBase
  let opcode = indexInf board' pos'
      instrCode = opcode `mod` 100
      paramModes = map (`mod` 10) (iterate (`div` 10) (opcode `div` 100))
      args = zipWith f (map (indexInf board') [(pos'+1)..]) paramModes
      f val = \case
        0 -> Left (fromIntegral val)
        1 -> Right val
        2 -> Left (fromIntegral val + relativeBase')
  modify $ over pos (+1)
  stepInstr instrCode args

evaluateUntilHaltWithInput :: [Integer] -> [Integer] -> ([Integer], XD)
evaluateUntilHaltWithInput inStack xs = runState go xd0 where
  go = do
    status <- evaluate
    case status of
      Halt -> return []
      Out x -> (x:) <$> go
      Continue -> go
  xd0 = XD (fromList xs) 0 0 inStack
