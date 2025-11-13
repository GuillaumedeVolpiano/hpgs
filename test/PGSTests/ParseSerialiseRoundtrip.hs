module PGSTests.ParseSerialiseRoundtrip (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Parser as P
import PGS.Utils

tests :: TestTree
tests = testGroup "Roundtrip"
  [ test_roundtrip_empty
  ]

test_roundtrip_empty :: TestTree
test_roundtrip_empty =
  testCase "parse . serialise = id for empty PGS" $ do
    let pgs = mempty
        bytes = S.unfold serialisePGS pgs
    result <- S.parse parsePGS bytes
    case result of
        Left (P.ParseError err) -> assertFailure err
        Right r  -> r @?= pgs
