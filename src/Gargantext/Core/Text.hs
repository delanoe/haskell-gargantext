{-|
Module      : Gargantext.Core.Text
Description : Ngrams tools
Copyright   : (c) CNRS, 2018
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Text gathers terms in unit of contexts.

-}


module Gargantext.Core.Text
  where

import Data.Text (Text, split)
import Gargantext.Prelude hiding (filter)
import NLP.FullStop (segment)
import qualified Data.Text as DT

-----------------------------------------------------------------
class HasText h
  where
    hasText :: h -> [Text]

-----------------------------------------------------------------
-- French words to distinguish contexts
newtype Texte      = Texte      Text
newtype Paragraphe = Paragraphe Text
newtype Phrase     = Phrase     Text
newtype MultiTerme = MultiTerme Text
newtype Mot        = Mot        Text
newtype Lettre     = Lettre     Text

-- | Type syn seems obvious
type    Titre      = Phrase

-----------------------------------------------------------------

instance Show Texte where
  show (Texte t) = show t

instance Show Paragraphe where
  show (Paragraphe p) = show p

instance Show Phrase where
  show (Phrase p) = show p

instance Show MultiTerme where
  show (MultiTerme mt) = show mt

instance Show Mot where
  show (Mot t) = show t

instance Show Lettre where
  show (Lettre l) = show l

-----------------------------------------------------------------

class Collage sup inf where
  dec ::  sup  -> [inf]
  inc :: [inf] -> sup

instance Collage Texte Paragraphe where
  dec (Texte t) = map Paragraphe $ DT.splitOn "\n" t
  inc           = Texte . DT.intercalate "\n" . map (\(Paragraphe t) -> t)

instance Collage Paragraphe Phrase where
  dec (Paragraphe t) = map Phrase $ sentences t
  inc                = Paragraphe . DT.unwords . map (\(Phrase p) -> p)

instance Collage Phrase MultiTerme where
  dec (Phrase t) = map MultiTerme $ DT.words t
  inc            = Phrase . DT.unwords . map (\(MultiTerme p) -> p)

instance Collage MultiTerme Mot where
  dec (MultiTerme mt) = map Mot $ DT.words mt
  inc                 = MultiTerme . DT.intercalate " " . map (\(Mot m) -> m)

-------------------------------------------------------------------
-- Contexts of text
sentences :: Text -> [Text]
sentences txt = map DT.pack $ segment $ DT.unpack txt

sentences' :: Text -> [Text]
sentences' txt = split isCharStop txt

isCharStop :: Char -> Bool
isCharStop c = c `elem` ['.','?','!']

unsentences :: [Text] -> Text
unsentences txts = DT.intercalate " " txts

-- | Ngrams size
size :: Text -> Int
size t = 1 + DT.count " " t



