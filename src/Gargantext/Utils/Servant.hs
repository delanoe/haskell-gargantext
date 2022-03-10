module Gargantext.Utils.Servant where

import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Csv (encodeDefaultOrderedByName, DefaultOrdered, ToNamedRecord)
import Network.HTTP.Media ((//), (/:))
import qualified Prelude
import Protolude
import Protolude.Partial (read)
import Servant

data CSV = CSV

instance Accept CSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance (DefaultOrdered a, ToNamedRecord a) => MimeRender CSV [a] where
  mimeRender _ val = encodeDefaultOrderedByName val

instance Read a => MimeUnrender CSV a where
   mimeUnrender _ bs = case BSC.take len bs of
     "text/csv" -> return . read . BSC.unpack $ BSC.drop len bs
     _ -> Left "didn't start with the magic incantation"
     where
       len :: Int64
       len = fromIntegral $ length ("text/csv" :: Prelude.String)

--instance ToNamedRecord a => MimeRender CSV [a] where
--  mimeRender _ val = encode val
