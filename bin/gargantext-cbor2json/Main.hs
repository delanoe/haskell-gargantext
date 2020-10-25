import Prelude (IO, id, (.))
import Data.Aeson (encode)
import Codec.Serialise (deserialise)
import qualified Data.ByteString.Lazy as L

import Gargantext.API.Ngrams.Types (NgramsRepo)

main :: IO ()
main = L.interact (encode . (id :: NgramsRepo -> NgramsRepo) . deserialise)
