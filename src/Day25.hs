module Day25 (main25) where

import IntCode (evaluateUntilHaltWithInput, parse)
import Data.Char

main25 :: IO ()
main25 = do
  input <- parse <$> readFile "res/input25"
  interact (map (chr . fromIntegral) . flip evaluateUntilHaltWithInput input . map (fromIntegral . ord))
