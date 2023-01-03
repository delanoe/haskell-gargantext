module Gargantext.Utils.Servant where

import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Csv (defaultEncodeOptions, encodeByNameWith, encodeDefaultOrderedByName, header, namedRecord, (.=), DefaultOrdered, EncodeOptions(..), NamedRecord, Quoting(QuoteNone), ToNamedRecord)
import qualified Data.Map as Map
import qualified Data.Text as T
import Gargantext.API.Ngrams.Types (mSetToList, NgramsRepoElement(..), NgramsTableMap, NgramsTerm(..), unNgramsTerm)
import Gargantext.Core.Types.Main (ListType(..))
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

-- CSV:
-- header: status\tlabel\tforms
-- item: map\taccountability\taccounting|&|accoutns|&|account
instance MimeRender CSV NgramsTableMap where
  -- mimeRender _ _val = encode ([] :: [(Text, Text)])
  mimeRender _ val = encodeByNameWith encOptions (header ["status", "label", "forms"]) $ fn <$> Map.toList val
    where
      encOptions = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t')
                                        , encQuoting = QuoteNone }
      fn :: (NgramsTerm, NgramsRepoElement) -> NamedRecord
      fn (NgramsTerm term, NgramsRepoElement { _nre_list, _nre_children }) =
        namedRecord [ "status" .= toText _nre_list
                    , "label" .= term
                    , "forms" .= (T.intercalate "|&|" $ unNgramsTerm <$> mSetToList _nre_children)]
      toText :: ListType -> Text
      toText CandidateTerm = "candidate"
      toText MapTerm = "map"
      toText StopTerm = "stop"

instance Read a => MimeUnrender CSV a where
   mimeUnrender _ bs = case BSC.take len bs of
     "text/csv" -> return . read . BSC.unpack $ BSC.drop len bs
     _ -> Left "didn't start with the magic incantation"
     where
       len :: Int64
       len = fromIntegral $ length ("text/csv" :: Prelude.String)

--instance ToNamedRecord a => MimeRender CSV [a] where
--  mimeRender _ val = encode val
