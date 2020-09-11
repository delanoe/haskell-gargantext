{-|
Module      : Gargantext.Core.Text.Ngrams
Description : Ngrams definition and tools
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

An @n-gram@ is a contiguous sequence of n items from a given sample of
text. In Gargantext application the items are words, n is a non negative
integer.

Using Latin numerical prefixes, an n-gram of size 1 is referred to as a
"unigram"; size 2 is a "bigram" (or, less commonly, a "digram"); size
3 is a "trigram". English cardinal numbers are sometimes used, e.g.,
"four-gram", "five-gram", and so on.

Source: https://en.wikipedia.org/wiki/Ngrams

TODO
group Ngrams -> Tree
compute occ by node of Tree
group occs according groups

compute cooccurrences
compute graph

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Gargantext.Core.Text.Terms
  where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Traversable
import GHC.Base (String)
import GHC.Generics (Generic)
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Core.Flow.Types
import Gargantext.Prelude
import Gargantext.Core.Text (sentences, HasText(..))
import Gargantext.Core.Text.Terms.Eleve (mainEleveWith, Tries, Token, buildTries, toToken)
import Gargantext.Database.Schema.Ngrams (Ngrams(..), NgramsType(..))
import Gargantext.Core.Text.Terms.Mono  (monoTerms)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Core.Text.Terms.Mono.Token.En (tokenize)
import Gargantext.Core.Text.Terms.Multi (multiterms)
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Text as Text

data TermType lang
  = Mono      { _tt_lang :: !lang }
  | Multi     { _tt_lang :: !lang }
  | MonoMulti { _tt_lang :: !lang }
  | Unsupervised { _tt_lang  :: !lang
                 , _tt_windowSize :: !Int
                 , _tt_ngramsSize :: !Int
                 , _tt_model      :: !(Maybe (Tries Token ()))
                 }
  deriving Generic

makeLenses ''TermType
--group :: [Text] -> [Text]
--group = undefined

-- remove Stop Words
-- map (filter (\t -> not . elem t)) $ 
------------------------------------------------------------------------
-- | Sugar to extract terms from text (hiddeng mapM from end user).
--extractTerms :: Traversable t => TermType Lang -> t Text -> IO (t [Terms])
extractTerms :: TermType Lang -> [Text] -> IO [[Terms]]

extractTerms (Unsupervised l n s m) xs = mapM (terms (Unsupervised l n s (Just m'))) xs
  where
    m' = case m of
      Just m''-> m''
      Nothing -> newTries n (Text.intercalate " " xs)

extractTerms termTypeLang xs = mapM (terms termTypeLang) xs


------------------------------------------------------------------------
withLang :: HasText a
         => TermType Lang
         -> [DocumentWithId a]
         -> TermType Lang
withLang (Unsupervised l n s m) ns = Unsupervised l n s m'
  where
    m' = case m of
      Nothing -> -- trace ("buildTries here" :: String)
               Just $ buildTries n ( fmap toToken
                                   $ uniText
                                   $ Text.intercalate " . "
                                   $ List.concat
                                   $ map hasText ns
                                   )
      just_m -> just_m
withLang l _ = l

------------------------------------------------------------------------
class ExtractNgramsT h
  where
    extractNgramsT :: HasText h
                   => TermType Lang
                   -> h
                   -> Cmd err (Map Ngrams (Map NgramsType Int))

filterNgramsT :: Int -> Map Ngrams (Map NgramsType Int)
                     -> Map Ngrams (Map NgramsType Int)
filterNgramsT s ms = Map.fromList $ map (\a -> filter' s a) $ Map.toList ms
  where
    filter' s' (ng@(Ngrams t n),y) = case (Text.length t) < s' of
          True  -> (ng,y)
          False -> (Ngrams (Text.take s' t) n , y)


-- =======================================================

-- | Terms from Text
-- Mono : mono terms
-- Multi : multi terms
-- MonoMulti : mono and multi
-- TODO : multi terms should exclude mono (intersection is not empty yet)
terms :: TermType Lang -> Text -> IO [Terms]
terms (Mono      lang) txt = pure $ monoTerms lang txt
terms (Multi     lang) txt = multiterms lang txt
terms (MonoMulti lang) txt = terms (Multi lang) txt
terms (Unsupervised lang n s m) txt = termsUnsupervised (Unsupervised lang n s (Just m')) txt
  where
    m' = maybe (newTries n txt) identity m
-- terms (WithList  list) txt = pure . concat $ extractTermsWithList list txt
------------------------------------------------------------------------

text2term :: Lang -> [Text] -> Terms
text2term _ [] = Terms [] Set.empty
text2term lang txt = Terms txt (Set.fromList $ map (stem lang) txt)

isPunctuation :: Text -> Bool
isPunctuation x = List.elem x $  (Text.pack . pure)
                             <$> ("!?(),;." :: String)

-- | Unsupervised ngrams extraction
-- language agnostic extraction
-- TODO: remove IO
-- TODO: newtype BlockText

type WindowSize = Int
type MinNgramSize = Int

termsUnsupervised :: TermType Lang -> Text -> IO [Terms]
termsUnsupervised (Unsupervised l n s m) =
               pure
             . map (text2term l)
             . List.nub
             . (List.filter (\l' -> List.length l' >= s))
             . List.concat
             . mainEleveWith (maybe (panic "no model") identity m) n
             . uniText
termsUnsupervised _ = undefined

newTries :: Int -> Text -> Tries Token ()
newTries n t = buildTries n (fmap toToken $ uniText t)

-- | TODO removing long terms > 24
uniText :: Text -> [[Text]]
uniText = map (List.filter (not . isPunctuation))
        . map tokenize
        . sentences       -- TODO get sentences according to lang
        . Text.toLower

