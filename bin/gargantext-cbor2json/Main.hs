
import System.Environment (getArgs)
import Prelude (IO, id, (.), ($))
import Data.Aeson (encode)
import Codec.Serialise (deserialise)
import qualified Data.ByteString.Lazy as L
import Gargantext.Core.NodeStory (NodeListStory)

main :: IO ()
main = L.interact (encode . (id :: NodeListStory -> NodeListStory) . deserialise)
