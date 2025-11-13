{-# LANGUAGE TupleSections #-}
module PGS.Utils (
                   parsePGS
                 , serialisePGS
                 , shiftPGS)
where

import           Control.Monad        (unless, void)
import           Data.Bits            (shiftL, (.|.), (.&.), shiftR)
import           Data.Sequence        (Seq ((:<|), (:|>)), ViewL (EmptyL, (:<)),
                                       singleton, viewl, (><))
import qualified Data.Sequence        as Sq (drop, null, splitAt)
import           Data.Word            (Word8)
import           PGS.Types            (DisplaySet, PGHeader (PGHeader),
                                       PGPack (PGPack), PGS, header, pts,
                                       segSize, segType, segments, dts)
import qualified Streamly.Data.Fold   as SF (foldl', foldr', toList)
import           Streamly.Data.Fold   (Fold)
import           Streamly.Data.Parser (Parser, many, manyTill, one, satisfy,
                                       takeEQ)
import qualified Data.List as L (unfoldr)
import Streamly.Data.Unfold (Unfold, lmap, unfoldEach)
import qualified Streamly.Data.Unfold as U (fromList, unfoldr)
import PGS.Constants (endPGPack)

parsePGS :: Monad m => Parser Word8 m PGS
parsePGS = Sq.drop 1 <$> many parseDisplaySet toSets

parsePack ::  Monad m => Parser Word8 m PGPack
parsePack = do
  h <- parseHeader
  let sz = segSize h
  s <- takeEQ sz SF.toList
  pure . PGPack h $ s

parseHeader :: Monad m => Parser Word8 m PGHeader
parseHeader = do
  void . satisfy $ (== 0x50)
  void . satisfy $ (== 0x47)
  pt <- takeEQ 4 foldToInt
  dt <- takeEQ 4 foldToInt
  st <- one
  PGHeader pt dt st <$> takeEQ 2 foldToInt

foldToInt :: Monad m => Fold m Word8 Int
foldToInt = SF.foldl' (\b a -> (b `shiftL` 8) .|. fromIntegral a ) 0

parseDisplaySet :: Monad m => Parser Word8 m DisplaySet
parseDisplaySet = manyTill parsePack end toSeq
  where
    toSeq = SF.foldr' (:<|) mempty 

end :: Monad m => Parser Word8 m ()
end = parseHeader >>= \h -> unless (segType h == 0x80) $ fail "not an End segment"

toSets :: Monad m => Fold m DisplaySet PGS
toSets = SF.foldr' groupSets . singleton $ []
  where
    groupSets ds sq = case viewl sq of
                        EmptyL -> error "Where did you get that empty list from?"
                        (h :< r) -> if any ((== 0x15) . segType . header) ds then mempty :<| (ds : h) :<| r
                                                                               else (ds : h) :<| r

shiftPGS :: [(Int, Int)] -> PGS -> PGS
shiftPGS shifts pgs = snd . foldl' shiftSeq (pgs, mempty) $ shifts
  where
    shiftSeq (sq, acc) (size, ms)
      | Sq.null sq = error ("All subtitles are not covered by shifts\n" ++ show sq)
      | otherwise = (sq', acc')
      where
        (cur, sq') = Sq.splitAt size sq
        shifted = fmap (map (shiftDisplaySetBy ms)) cur
        acc' = acc >< shifted

shiftDisplaySetBy :: Int -> DisplaySet -> DisplaySet
shiftDisplaySetBy ms = fmap (shiftPackBy ms)

shiftPackBy :: Int -> PGPack -> PGPack
shiftPackBy ms p = p{header=shiftHeaderBy ms $ header p}

shiftHeaderBy :: Int -> PGHeader -> PGHeader
shiftHeaderBy ms h = h{pts = pts h + 90*ms}

serialisePGS :: (Monad m, Applicative m) => Unfold m PGS Word8
serialisePGS = unfoldEach serialiseDisplaySet $ unfoldEach U.fromList fromSeq

fromSeq :: (Monad m, Applicative m) => Unfold m (Seq a) a
fromSeq = U.unfoldr $ \s -> case viewl s of
                              EmptyL -> Nothing
                              (h :< r) -> Just (h, r)

serialiseDisplaySet :: (Monad m, Applicative m) => Unfold m DisplaySet Word8
serialiseDisplaySet = lmap (:|> endPGPack) $ unfoldEach serialisePGPack fromSeq 

serialisePGPack :: (Monad m, Applicative m) => Unfold m PGPack Word8
serialisePGPack = lmap (\pp -> serialiseHeader (header pp) ++ segments pp) U.fromList

serialiseHeader :: PGHeader -> [Word8]
serialiseHeader h = 0x50 : 0x47 : (intToWord8N 4 (pts h) ++ intToWord8N 4 (dts h) ++ (segType h : intToWord8N 2 (segSize h)))

serialiseInt :: Int -> [Word8]
serialiseInt = L.unfoldr $ \i -> Just (fromIntegral (i .&. 0xFF), i `shiftR` 8)

intToWord8N :: Int -> Int -> [Word8]
intToWord8N count = reverse . take count . serialiseInt
