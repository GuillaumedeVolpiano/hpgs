module Main where

import           Data.Function              ((&))
import           PGS.Utils                  (parsePGS, serialisePGS, shiftPGS)
import qualified Streamly.Data.Stream       as S (fold)
import           Streamly.Data.Stream       (parse, unfold)
import qualified Streamly.FileSystem.FileIO as F (read, write)
import           Types                      (fullDelay, getArgs, input, output, ratio)


main :: IO ()
main = do
  args <- getArgs
  let file = input args
      file' = output args
      ra = ratio args
  result <- F.read file & parse parsePGS
  case result of
    Left err -> error (show err)
    Right r -> do
      let shifts = either error id . fullDelay (length r) $ args
          shifted = unfold serialisePGS $ shiftPGS ra shifts r
      S.fold (F.write file') shifted

