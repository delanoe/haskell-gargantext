module Gargantext.Utils.Servant where

import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Csv (defaultEncodeOptions, encodeByNameWith, encodeDefaultOrderedByName, header, namedRecord, (.=), DefaultOrdered, EncodeOptions(..), NamedRecord, Quoting(QuoteNone), ToNamedRecord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
  mimeRender _ = encodeDefaultOrderedByName

instance MimeRender CSV T.Text where
  mimeRender _ = BSC.fromStrict . TE.encodeUtf8

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

----------------------------

data Markdown = Markdown

instance Accept Markdown where
  contentType _ = "text" // "markdown"

instance MimeRender Markdown T.Text where
  mimeRender _ = BSC.fromStrict . TE.encodeUtf8

instance MimeUnrender Markdown T.Text where
  mimeUnrender _ = Right . TE.decodeUtf8 . BSC.toStrict
