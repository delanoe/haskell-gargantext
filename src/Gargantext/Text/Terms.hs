{-|
Module      : Gargantext.Text.Ngrams
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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Text.Terms
  where

import Control.Lens
import Data.Text (Text)
import Data.Traversable
import GHC.Base (String)

import Gargantext.Prelude
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Text.Terms.Multi (multiterms)
import Gargantext.Text.Terms.Mono  (monoTerms)
import Gargantext.Text.Terms.Mono.Stem (stem)

import qualified Data.Set  as Set
import qualified Data.List as List
import qualified Data.Text as Text
import Gargantext.Text (sentences)
import Gargantext.Text.Terms.Mono.Token.En (tokenize)
import Gargantext.Text.Terms.Eleve (mainEleveWith, Tries, Token, buildTries, toToken)

data TermType lang
  = Mono      { _tt_lang :: lang }
  | Multi     { _tt_lang :: lang }
  | MonoMulti { _tt_lang :: lang }
  | Unsupervised { _tt_lang  :: lang
                 , _tt_windoSize  :: Int
                 , _tt_ngramsSize :: Int
                 , _tt_model :: Maybe (Tries Token ())
  }
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
             . (List.filter (\l' -> List.length l' > s))
             . List.concat
             . mainEleveWith (maybe (panic "no model") identity m) n
             . uniText
termsUnsupervised _ = undefined

newTries :: Int -> Text -> Tries Token ()
newTries n t = buildTries n (fmap toToken $ uniText t)

uniText :: Text -> [[Text]]
uniText = map (List.filter (not . isPunctuation))
        . map tokenize
        . sentences   -- | TODO get sentences according to lang


