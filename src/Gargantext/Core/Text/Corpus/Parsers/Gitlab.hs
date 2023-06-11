module Gargantext.Core.Text.Corpus.Parsers.Gitlab (
  Issue(..), gitlabIssue2hyperdataDocument, readFile_Issues, readFile_IssuesAsDocs
) where

import Data.Aeson
import Data.Time
import qualified Data.Text            as DT
import qualified Data.ByteString.Lazy as DBL
import System.FilePath (FilePath)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

data Issue = Issue { _issue_id      :: !Int
                   , _issue_title   :: !DT.Text
                   , _issue_content :: !DT.Text
                   , _issue_created :: !LocalTime
                   , _issue_closed  :: !(Maybe UTCTime)
                   }
  deriving (Show)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
    <$> v .: "c0" -- id
    <*> v .: "c1" -- title
    <*> v .: "c2" -- content
    <*> v .: "c3" -- creation time
    <*> v .:? "c4" -- close time

gitlabIssue2hyperdataDocument :: Issue -> HyperdataDocument
gitlabIssue2hyperdataDocument issue = HyperdataDocument
    { _hd_bdd = Nothing
    , _hd_doi = Nothing
    , _hd_url = Nothing
    , _hd_uniqId = Nothing
    , _hd_uniqIdBdd = Nothing
    , _hd_page = Nothing
    , _hd_title = Just (_issue_title issue)
    , _hd_authors = Nothing
    , _hd_institutes = Nothing
    , _hd_source = Nothing
    , _hd_abstract = Just (_issue_content issue)
    , _hd_publication_date = Just $ DT.pack $ show date
    , _hd_publication_year = Just $ fromIntegral year
    , _hd_publication_month = Just month
    , _hd_publication_day = Just day
    , _hd_publication_hour = Just (todHour tod)
    , _hd_publication_minute = Just (todMin tod)
    , _hd_publication_second = Just (round $ todSec tod)
    , _hd_language_iso2 = Just $ (DT.pack . show) lang
    }
  where lang = EN
        date = _issue_created issue
        (year, month, day) = toGregorian $ localDay date
        tod = localTimeOfDay date

readFile_Issues :: FilePath -> IO [Issue]
readFile_Issues fp = do
  raw <- DBL.readFile fp
  let mayIssues = decode raw
  case mayIssues of
    Just is -> pure is
    Nothing -> pure []

readFile_IssuesAsDocs :: FilePath -> IO [HyperdataDocument]
readFile_IssuesAsDocs = fmap (fmap gitlabIssue2hyperdataDocument) . readFile_Issues
