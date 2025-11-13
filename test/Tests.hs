module Main (main) where

import Test.Tasty
import qualified PGSTests.ShiftTests as Shift
import qualified PGSTests.SerialiseTests as Serial
import qualified PGSTests.ParseSerialiseRoundtrip as Round
import qualified PGSTests.GoldenTests as Golden

main :: IO ()
main = defaultMain $ testGroup "hpgs tests"
  [ Shift.tests
  , Serial.tests
  , Round.tests
  , Golden.tests
  ]

