module Gargantext.Analysis where

import Gargantext.Prelude

-- import qualified Data.Text.Lazy as DTL
import Data.Text
import Opaleye (Column, PGInt4)
--import Data.Map    as DM
--import Data.Vector as DV

-- | Simple function to count Occurrences in a context of text.
occOfDocument :: Column PGInt4 -> Text -> IO Int
occOfDocument = undefined
--occOfDocument c_id txt = do
--    docs <- pm (hyperdataDocument_Abstract . node_hyperdata) <$> getCorpusDocument c_id
--    let occs = pm (\x -> maybe ""  identity x) docs
--    let result = case sequence $ pm (parseOccurrences  txt) occs of
--            -- TODO find a way to get nice message d'errors (file, function, line)
--            Left str -> error $ "[ERRROR] at file/function/line" ++ str
--            Right xs -> xs
--    pure (sum result)



data Occurrences a b = Map a b









