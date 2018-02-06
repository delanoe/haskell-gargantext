module Gargantext.RCT where

import Gargantext.Prelude

foo :: Int
foo = undefined
--import Data.Text (Text, words)
--import Data.Attoparsec.Text (anyChar, isEndOfLine, Parser, takeTill, many1, endOfLine, space, manyTill)
--import Control.Applicative (many)

-- RCT is the acronym for Referential ConText (of Text)
-- at the begin there was a byte
-- then a char 
-- Char -> RCT [Char]

-- then a list of chars called a string, we call it a Form
-- (removing all weird charachters which are not alphanumeric)

-- Form -> RCT Sentence

-- These forms compose the RCT Sentence
-- an ngrams is composed with multiple forms

-- Paragraph = [Sentence]

-- type Title = Paragraph
-- data Block = [Paragraph]
-- Block is taken form Pandoc

-- data Document = [Block]

-- Set of databases
-- Database
-- Set of Articles
--      Article
--      Paragraph (abstract + title)
-- Sentence - Ngrams - Forms



--separateurs :: Parser Text
--separateurs = dropWhile isEndOfLine

--paragraphs :: Parser [Text]
--paragraphs = many paragraph
--
--paragraph :: Parser Text
--paragraph = takeTill isEndOfLine <* many1 endOfLine
--
-- forms :: Text -> [Text]
-- forms = words



