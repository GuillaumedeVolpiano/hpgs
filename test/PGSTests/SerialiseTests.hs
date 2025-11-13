module PGSTests.SerialiseTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import PGS.Types
import PGS.Utils

tests :: TestTree
tests = testGroup "Serialisation"
  [ test_intToWord8N
  , test_serialiseHeader
  , test_serialisePGPack
  ]

test_intToWord8N :: TestTree
test_intToWord8N = testCase "intToWord8N produces big-endian padded" $
  intToWord8N 4 0x123456 @?= [0x00,0x12,0x34,0x56]

test_serialiseHeader :: TestTree
test_serialiseHeader = testCase "serialiseHeader structure" $ serialiseHeader h @?=
        [ 0x50, 0x47 ] ++
        intToWord8N 4 1000 ++
        intToWord8N 4 2000 ++
        [0x15] ++
        intToWord8N 2 42
  where
    h = PGHeader 1000 2000 0x15 42

test_serialisePGPack :: TestTree
test_serialisePGPack = testCase "serialise single PGPack" $ out @?= ws
  where
      h  = PGHeader 1 2 0x16 3
      pp = PGPack h [10,20,30]
      ws = serialiseHeader h
      out = take (length ws) (serialiseHeader h ++ segments pp)
