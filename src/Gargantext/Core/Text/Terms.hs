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

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Text.Terms
  where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable
import GHC.Base (String)
import GHC.Generics (Generic)
import qualified Data.List           as List
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.HashMap.Strict as HashMap
import Gargantext.Core
import Gargantext.Core.Utils (groupWithCounts)
import Gargantext.Core.Text (sentences, HasText(..))
import Gargantext.Core.Text.Terms.Eleve (mainEleveWith, Tries, Token, buildTries, toToken)
import Gargantext.Core.Text.Terms.Mono  (monoTerms)
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Core.Text.Terms.Mono.Token.En (tokenize)
import Gargantext.Core.Text.Terms.Multi (multiterms)
import Gargantext.Core.Types
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Ngrams (insertNgrams)
import Gargantext.Database.Query.Table.NgramsPostag (NgramsPostag(..), insertNgramsPostag, np_form, np_lem)
import Gargantext.Database.Schema.Ngrams (Ngrams(..), NgramsType(..), ngramsTerms, text2ngrams, NgramsId)
import Gargantext.Prelude

data TermType lang
  = Mono      { _tt_lang :: !lang }
  | Multi     { _tt_lang :: !lang }
  | MonoMulti { _tt_lang :: !lang }
  | Unsupervised { _tt_lang       :: !lang
                 , _tt_windowSize :: !Int
                 , _tt_ngramsSize :: !Int
                 , _tt_model      :: !(Maybe (Tries Token ()))
                 }
  deriving (Generic)
deriving instance (Show lang) => Show (TermType lang)

makeLenses ''TermType
--group :: [Text] -> [Text]
--group = undefined

-- remove Stop Words
-- map (filter (\t -> not . elem t)) $
------------------------------------------------------------------------
-- | Sugar to extract terms from text (hidding 'mapM' from end user).
--extractTerms :: Traversable t => TermType Lang -> t Text -> IO (t [Terms])
extractTerms :: NLPServerConfig -> TermType Lang -> [Text] -> IO [[TermsWithCount]]
extractTerms ncs (Unsupervised {..}) xs = mapM (terms ncs (Unsupervised { _tt_model = Just m', .. })) xs
  where
    m' = case _tt_model of
      Just m''-> m''
      Nothing -> newTries _tt_windowSize (Text.intercalate " " xs)
extractTerms ncs termTypeLang xs = mapM (terms ncs termTypeLang) xs


------------------------------------------------------------------------
withLang :: (Foldable t, Functor t, HasText h)
         => TermType Lang
         -> t h
         -> TermType Lang
withLang (Unsupervised {..}) ns = Unsupervised { _tt_model = m', .. }
  where
    m' = case _tt_model of
      Nothing -> -- trace ("buildTries here" :: String)
               Just $ buildTries _tt_ngramsSize
                    $ fmap toToken
                    $ uniText
                    $ Text.intercalate " . "
                    $ List.concat
                    $ map hasText ns
      just_m -> just_m
withLang l _ = l

------------------------------------------------------------------------
data ExtractedNgrams = SimpleNgrams   { unSimpleNgrams   :: Ngrams       }
                     | EnrichedNgrams { unEnrichedNgrams :: NgramsPostag }
  deriving (Eq, Ord, Generic, Show)

instance Hashable ExtractedNgrams

-- | A typeclass that represents extracting ngrams from an entity.
class ExtractNgramsT h
  where
    extractNgramsT :: HasText h
                   => TermType Lang
                   -> h
                   -> Cmd err (HashMap ExtractedNgrams (Map NgramsType Int, TermsCount))
------------------------------------------------------------------------
enrichedTerms :: Lang -> PosTagAlgo -> POS -> Terms -> NgramsPostag
enrichedTerms l pa po (Terms ng1 ng2) =
  NgramsPostag l pa po form lem
    where
      form = text2ngrams $ Text.intercalate " " ng1
      lem  = text2ngrams $ Text.intercalate " " $ Set.toList ng2

------------------------------------------------------------------------
cleanNgrams :: Int -> Ngrams -> Ngrams
cleanNgrams s ng
      | Text.length (ng ^. ngramsTerms) < s = ng
      | otherwise                           = text2ngrams (Text.take s (ng ^. ngramsTerms))

cleanExtractedNgrams :: Int -> ExtractedNgrams -> ExtractedNgrams
cleanExtractedNgrams s (SimpleNgrams   ng) = SimpleNgrams $ (cleanNgrams s) ng
cleanExtractedNgrams s (EnrichedNgrams ng) = EnrichedNgrams $ over np_form (cleanNgrams s)
                                                            $ over np_lem  (cleanNgrams s) ng

extracted2ngrams :: ExtractedNgrams -> Ngrams
extracted2ngrams (SimpleNgrams   ng) = ng
extracted2ngrams (EnrichedNgrams ng) = view np_form ng

---------------------------
insertExtractedNgrams :: [ ExtractedNgrams ] -> Cmd err (HashMap Text NgramsId)
insertExtractedNgrams ngs = do
  let (s, e) = List.partition isSimpleNgrams ngs
  m1 <- insertNgrams       (map unSimpleNgrams   s)
  --printDebug "others" m1

  m2 <- insertNgramsPostag (map unEnrichedNgrams e)
  --printDebug "terms" m2

  let result = HashMap.union m1 m2
  pure result

isSimpleNgrams :: ExtractedNgrams -> Bool
isSimpleNgrams (SimpleNgrams _) = True
isSimpleNgrams _                = False

------------------------------------------------------------------------
-- | Terms from 'Text'
-- 'Mono' : mono terms
-- 'Multi' : multi terms
-- 'MonoMulti' : mono and multi
-- TODO : multi terms should exclude mono (intersection is not empty yet)
terms :: NLPServerConfig -> TermType Lang -> Text -> IO [TermsWithCount]
terms _   (Mono      lang) txt = pure $ monoTerms lang txt
terms ncs (Multi     lang) txt = multiterms ncs lang txt
terms ncs (MonoMulti lang) txt = terms ncs (Multi lang) txt
terms _   (Unsupervised { .. }) txt = pure $ termsUnsupervised (Unsupervised { _tt_model = Just m', .. }) txt
  where
    m' = maybe (newTries _tt_ngramsSize txt) identity _tt_model
-- terms (WithList  list) txt = pure . concat $ extractTermsWithList list txt


------------------------------------------------------------------------
type WindowSize = Int
type MinNgramSize = Int

-- | Unsupervised ngrams extraction
-- language agnostic extraction
-- TODO: newtype BlockText
termsUnsupervised :: TermType Lang -> Text -> [TermsWithCount]
termsUnsupervised (Unsupervised { _tt_model = Nothing }) = panic "[termsUnsupervised] no model"
termsUnsupervised (Unsupervised { _tt_model = Just _tt_model, .. }) =
               map (\(t, cnt) -> (text2term _tt_lang t, cnt))
             . groupWithCounts
             -- . List.nub
             . (List.filter (\l' -> List.length l' >= _tt_windowSize))
             . List.concat
             . mainEleveWith _tt_model _tt_ngramsSize
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

text2term :: Lang -> [Text] -> Terms
text2term _ [] = Terms [] Set.empty
text2term lang txt = Terms txt (Set.fromList $ map (stem lang) txt)

isPunctuation :: Text -> Bool
isPunctuation x = List.elem x $  (Text.pack . pure)
                             <$> ("!?(),;.:" :: String)
