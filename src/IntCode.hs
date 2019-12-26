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

data XD = XD { _board :: Seq Integer, _pos :: Int, _relativeBase :: Int }
data StatusInt = InInt (Integer -> State XD StatusInt) | OutInt Integer | ContinueInt | HaltInt

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

stepInstr :: Integer -> [Either Int Integer] -> State XD StatusInt
stepInstr opcode (a:b:c:_) = do
  board' <- gets _board 
  let eval = either (indexInf board') id 
  let binInstr f = do
        modify $ over board (updateInf (fromLeft c) (f (eval a) (eval b)))
        modify $ over pos (+3)
        return ContinueInt
  let jump f = do
        modify $ over pos (if f (eval a) then const (fromIntegral $ eval b) else (+2))
        return ContinueInt
  let cmp f = do
        modify $ over board (updateInf (fromLeft c) (if f (eval a) (eval b) then 1 else 0))
        modify $ over pos (+3)
        return ContinueInt
  case opcode of 
    1 -> binInstr (+)
    2 -> binInstr (*)
    3 -> do
      return $ InInt $ \inputValue -> do
        modify $ over board (updateInf (fromLeft a) inputValue)
        modify $ over pos (+1)
        return ContinueInt
    4 -> do
      modify $ over pos (+1)
      return (OutInt (eval a))
    5 -> jump (/= 0)
    6 -> jump (== 0)
    7 -> cmp (<)
    8 -> cmp (==)
    9 -> do
      modify $ over relativeBase (+ (fromIntegral $ eval a))
      modify $ over pos (+1)
      return ContinueInt
    99 -> return HaltInt

evaluate :: State XD StatusInt
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

data ExtStatus = Out Integer ExtStatus | In (Integer -> ExtStatus) | Halt XD

instance Show ExtStatus where
  show = \case
    Out o e -> "Out " ++ show o ++ " (" ++ show e ++ ")"
    In _ -> "In"
    Halt _ -> "Halt"

run :: [Integer] -> ExtStatus
run xs = evalState go xd0
  where
    go = do
      status <- evaluate
      xd <- get
      case status of
        HaltInt -> return $ Halt xd
        OutInt x -> (Out x) <$> go
        InInt f -> return $ In $ \input -> flip evalState xd $ do
          f input
          go
        ContinueInt -> go
    xd0 = XD (fromList xs) 0 0

evaluateUntilHaltWithInput :: [Integer] -> [Integer] -> [Integer]
evaluateUntilHaltWithInput inStack xs = go inStack (run xs)
  where
    go ins = \case
      In f -> go (tail ins) (f (head ins))
      Out out nxt -> out : go ins nxt
      Halt _ -> []

feed :: ExtStatus -> [Integer] -> ExtStatus
feed ex [] = ex
feed (In g) (x : xs) = feed (g x) xs

expect :: Int -> ExtStatus -> ([Integer], ExtStatus)
expect 0 ex = ([], ex)
expect n (Out out nxt) = let (rest, final) = expect (n-1) nxt
                          in (out : rest, final)
