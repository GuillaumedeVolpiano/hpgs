{-# LANGUAGE GADTs #-}
module PGS.Types (
                   PGS
                 , PGPack (PGPack)
                 , header
                 , segments
                 , PGHeader (PGHeader)
                 , pts
                 , dts
                 , segType
                 , segSize
                 , DisplaySet
                 )
where
import           Data.Word (Word8)
import Data.Sequence (Seq)

type PGS = Seq [DisplaySet]

data PGPack where
  PGPack :: {header :: PGHeader, segments :: [Word8]} -> PGPack deriving Show

data PGHeader where
  PGHeader :: {pts :: PTS,
                 dts :: DTS,
                 segType :: Word8,
                 segSize :: Int} ->
                PGHeader deriving Show

type PTS = Int
type DTS = Int
type DisplaySet = Seq PGPack

