{-|
Module      : Gargantext.Text.Parsers.WOS
Description : Parser for Wikimedia dump
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

@Gargantext.Text.Parsers.Wikimedia@:
This module provide a parser for wikipedia dump.
This include an xml parser for wikipedia's xml
and an wikimedia to plaintext converter for the wikipedia text field
-}

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
import Data.Either

-- | Use case
-- >>> :{
--  wikimediaFile <- BL.readFile "text.xml"
--  _ <- runConduit $ parseLBS def wikimediaFile
--        .| force "mediawiki required" parseMediawiki
--        .| CL.mapM mediawikiPageToPlain
--        .| CL.mapM_ print
-- :}

-- | A simple "Page" type.
-- For the moment it takes only text and title
--  (since there is no abstract) will see if other data are relevant.
data Page = Page
  {
    _markupFormat :: MarkupFormat
  , _title :: Maybe T.Text
  , _text :: Maybe T.Text
  }
  deriving (Show)

data MarkupFormat = Mediawiki | Plaintext
  deriving (Show)

parseRevision :: MonadThrow m => ConduitT Event o m (Maybe T.Text)
parseRevision =
  tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}revision" $ do
  text <-
    force "text is missing" $ ignoreExcept
    "{http://www.mediawiki.org/xml/export-0.10/}text" content
  many_
    $ ignoreAnyTreeContent
  return text

-- | Utility function that match everything but the tag given
tagUntil :: Name -> NameMatcher Name
tagUntil name = matching (/= name)

-- | Utility function that parse nothing but the tag given,
-- usefull because we have to consume every data.
ignoreExcept :: MonadThrow m => Name
  -> ConduitT Event o m b
  -> ConduitT Event o m (Maybe b)
ignoreExcept name f = do
  _ <- consumeExcept name
  tagIgnoreAttrs (matching (==name)) f

-- | Utility function that consume everything but the tag given
-- usefull because we have to consume every data.
consumeExcept :: MonadThrow m => Name -> ConduitT Event o m ()
consumeExcept = many_ . ignoreTreeContent . tagUntil

parsePage :: MonadThrow m => ConduitT Event o m (Maybe Page)
parsePage =
  tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}page" $ do
  title <-
    tagNoAttr "{http://www.mediawiki.org/xml/export-0.10/}title" content
  _ <-
    consumeExcept "{http://www.mediawiki.org/xml/export-0.10/}revision"
  revision <-
    parseRevision
  many_ $ ignoreAnyTreeContent
  return $ Page Mediawiki title revision

parseMediawiki :: MonadThrow m => ConduitT Event Page m (Maybe ())
parseMediawiki =
  tagIgnoreAttrs "{http://www.mediawiki.org/xml/export-0.10/}mediawiki"
  $ manyYield' parsePage

-- | Convert a Mediawiki Page to a Plaintext Page.
-- Need to wrap the result in IO to parse and to combine it.
mediawikiPageToPlain :: Page -> IO Page
mediawikiPageToPlain page = do
  title <- mediaToPlain $ _title page
  revision <- mediaToPlain $ _text page
  return $ Page Plaintext title revision
  where mediaToPlain media =
          case media of
            (Nothing) -> return Nothing
            (Just med) -> do
              res <- runIO $ do
                doc <- readMediaWiki def med
                writePlain def doc
              case res of
                (Left _) -> return Nothing
                (Right r) -> return $ Just r
