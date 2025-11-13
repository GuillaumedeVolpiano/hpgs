module PGS.Constants (endPGPack)

where

import PGS.Types (PGPack(PGPack), segments, header, PGHeader(PGHeader))

endPGPack :: PGPack
endPGPack = PGPack {header = PGHeader 0 0 0x80 0, segments =[]}
