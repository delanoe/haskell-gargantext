{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Parsers.Wikimedia where
import Prelude (print)
import Gargantext.Prelude
import Text.XML.Stream.Parse
import Control.Monad.Catch
import Data.ByteString.Lazy
import Data.Conduit
import Data.XML.Types (Event)
import Data.Text as T

data Page = Page
  {
    _title :: T.Text
  , _text :: Maybe T.Text
  }
  deriving (Show)

runParser :: IO ()
runParser = do
  file <- readFile "text.xml"
  page <- runConduit $  parseLBS def file .| force "page required" parsePage
  print page

parseRevision :: MonadThrow m => ConduitT Event o m (Maybe T.Text)
parseRevision = tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}revision" $ do
  text <- force "text is missing" $ tagIgnoreAttrs "{http://www.mediawiki.org/xml/export-0.10/}text" content
  many_ $ ignoreAnyTreeContent
  return text

parsePage :: MonadThrow m => ConduitT Event o m (Maybe Page)
parsePage = tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}page" $ do
  title <- force "title is missing" $ tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}title" content
  revision <- parseRevision
  many_ $ ignoreAnyTreeContent
  return $ Page title revision

parseMediawiki :: MonadThrow m => ConduitT Event Page m (Maybe ())
parseMediawiki = tagIgnoreAttrs "{http://www.mediawiki.org/xml/export-0.10/}mediawiki" $ manyYield' parsePage

