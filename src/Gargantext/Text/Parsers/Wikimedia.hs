{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Parsers.Wikimedia where
import Gargantext.Prelude
import Text.XML.Stream.Parse
import Control.Monad.Catch
import Data.Conduit
import Data.XML.Types (Event, Name)
import Text.Pandoc
import Data.Text as T

-- | This module provide a parser for wikipedia dump.
-- This include an xml parser for wikipedia's xml and an wikimedia to plaintext converter for the wikipedia text field
-- | Use case
-- >>> :{
--  wikimediaFile <- BL.readFile "text.xml"
--  _ <- runConduit $ parseLBS def wikimediaFile .| force "mediawiki required" parseMediawiki .| CL.mapM mediawikiPageToPlain
-- :}

-- | A simple "Page" type, for the moment it take only text and title (since there is no abstract) will see if other datas are relevant.
data Page = Page
  {
    _title :: T.Text
  , _text :: T.Text
  }
  deriving (Show)

parseRevision :: MonadThrow m => ConduitT Event o m (Maybe T.Text)
parseRevision = tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}revision" $ do
  text <- force "text is missing" $ ignoreExcept "{http://www.mediawiki.org/xml/export-0.10/}text" content
  many_ $ ignoreAnyTreeContent
  return text

tagUntil :: Name -> NameMatcher Name
tagUntil name = matching (/= name)

-- | Utility function that parse nothing but the tag given, usefull because we have to consume every data.
ignoreExcept :: MonadThrow m => Name -> ConduitT Event o m b -> ConduitT Event o m (Maybe b)
ignoreExcept name f = do
  _ <- consumeExcept name
  tagIgnoreAttrs (matching (==name)) f

consumeExcept :: MonadThrow m => Name -> ConduitT Event o m ()
consumeExcept = many_ . ignoreTreeContent . tagUntil

parsePage :: MonadThrow m => ConduitT Event o m (Maybe Page)
parsePage = tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}page" $ do
  title <- force "title is missing" $ tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}title" content
  _ <- consumeExcept "{http://www.mediawiki.org/xml/export-0.10/}revision"
  revision <- force "revision is missing" $ parseRevision
  many_ $ ignoreAnyTreeContent
  return $ Page title revision

parseMediawiki :: MonadThrow m => ConduitT Event Page m (Maybe ())
parseMediawiki = tagIgnoreAttrs "{http://www.mediawiki.org/xml/export-0.10/}mediawiki" $ manyYield' parsePage

-- | Need to wrap the result in IO to parse and to combine it.
mediawikiPageToPlain :: Page -> IO Page
mediawikiPageToPlain page = do
  title <- mediaToPlain $ _title page
  revision <- mediaToPlain $ _text page
  return $ Page title revision
  where mediaToPlain media = do
          res <- runIO $ do
            doc <- readMediaWiki def media
            writePlain def doc
          handleError res
