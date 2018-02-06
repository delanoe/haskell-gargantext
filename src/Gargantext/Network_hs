module Data.Gargantext.Network where

import Data.Gargantext.Prelude

import Data.Map    as DM
import Data.Vector as DV

type Measure a b c = DM.Map a (DM.Map b c)


-- UTCTime Paire Granularity [Candle]

-- GargVector Paire Granularity [Candle]
type GargVector a b c     = DM.Map a ( DM.Map b c)

-- GargMatrix Granularity (Paire Paire) [Candle]
type GargMatrix a b c d   = DM.Map a (FolioVector b c d)

-- GargMatrix Granularity (Paire Paire) [Candle]
type GargTensor a b c d e = DM.Map a (FolioMatrix b c d e)



--data PortGarg = PortGarg { _portFolioParameters :: Parameters
--                           , _portGargData      :: Garg
--}


toMeasure :: Granularity -> Paire -> [Candle] 
           -> Measure Granularity Paire Candle
toMeasure g c1 c2 cs = DM.fromList [(g, 





