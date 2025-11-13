module PGSTests.GoldenTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Parser as P
import qualified Streamly.FileSystem.FileIO as F
import qualified Streamly.FileSystem.Path as F

import PGS.Utils (parsePGS, serialisePGS)
import PGS.Types (PGS, PGPack(PGPack), PGHeader(PGHeader))
import Data.Function ((&))
import Data.Word
import PGSTests.Expected (expectedPGS)

goldenBytes :: [Word8]
goldenBytes =
    [ 0x50,0x47, 0,0,0,1, 0,0,0,2, 0x15, 0,3, 1,2,3
    , 0x50,0x47, 0,0,0,0, 0,0,0,0, 0x80, 0,0
    , 0x50,0x47, 0,0,0,3, 0,0,0,4, 0x16, 0,2, 9,9
    , 0x50,0x47, 0,0,0,0, 0,0,0,0, 0x80, 0,0
    ]

tests :: TestTree
tests = testGroup "Golden tests"
    [ testParseGolden
    , testSerialiseGolden
    ]

goldenPath :: F.Path
goldenPath = F.fromString_ "test/golden/sample.sup"

testParseGolden :: TestTree
testParseGolden = testCase "parse golden sample.pgs" $ do
    res <- F.read goldenPath & S.parse parsePGS
    case res of
        Left (P.ParseError err) -> assertFailure err
        Right pgs           -> pgs @?= expectedPGS 

testSerialiseGolden :: TestTree
testSerialiseGolden = testCase "serialise + parse golden" $ do
    res   <- S.fromList goldenBytes & S.parse parsePGS 
    case res of
        Left (P.ParseError err) -> assertFailure err
        Right pgs -> do
          spgs <- S.unfold serialisePGS pgs & S.toList
          spgs @?= goldenBytes

