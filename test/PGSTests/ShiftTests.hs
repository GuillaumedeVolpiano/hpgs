module PGSTests.ShiftTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Sequence (fromList)
import Data.Ratio ((%))
import PGS.Types
import PGS.Utils

tests :: TestTree
tests = testGroup "Shifting"
  [ test_shiftHeaderBy
  , test_shiftDisplaySetBy
  , test_shiftPGS
  ]

test_shiftHeaderBy :: TestTree
test_shiftHeaderBy =
  testCase "shiftHeaderBy applies ratio and ms" $ pts h' @?= round (r * 1000) + 9000 
    where
      h = PGHeader 1000 2000 0x15 20
      r = 25 % 24 :: Rational
      h' = shiftHeaderBy r 100 h

test_shiftDisplaySetBy :: TestTree
test_shiftDisplaySetBy =
  testCase "shiftDisplaySetBy maps shift over packs" $ all ((== 10 + 90) . pts . header) ds' @? "each pts should shift by 90ms"
    where
      p = PGPack (PGHeader 10 20 0x15 0) []
      ds = fromList [p,p,p]
      ds' = shiftDisplaySetBy 1 1 ds

test_shiftPGS :: TestTree
test_shiftPGS =
  testCase "shiftPGS splits PGS and applies shifts" $ length out @?= 3
    where
      p  = PGPack (PGHeader 100 0 0x15 0) []
      ds = fromList [p]
      pgs = fromList [ [ds], [ds], [ds] ]  -- three display sets
      shifts = [(2,1), (1,2)]
      out = shiftPGS 1 shifts pgs
