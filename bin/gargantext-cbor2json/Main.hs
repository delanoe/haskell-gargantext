
import System.Environment (getArgs)
import Prelude (IO, id, (.), ($))
import Data.Aeson (encode)
import Codec.Serialise (deserialise)
import qualified Data.ByteString.Lazy as L
import Gargantext.Core.NodeStory (NodeListStory)

main :: IO ()
main = do
  [inFile,outFile] <- getArgs
  inData <- L.readFile inFile
  let outData = (encode . (id :: NodeListStory -> NodeListStory) . deserialise) inData
  L.writeFile outData
