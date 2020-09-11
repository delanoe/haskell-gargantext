import Prelude (IO, id, (.))
import Codec.Serialise (deserialise)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L
import Gargantext.API.Ngrams (NgramsRepo)

main :: IO ()
main = L.interact (encode . (id :: NgramsRepo -> NgramsRepo) . deserialise)
