module Gargantext.Core.Text.Corpus.Parsers.FrameWrite where

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Maybe
import Data.Text
import Gargantext.Prelude
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String


-- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/331

-- title : everything above the first ==
-- Authors : default : anonymous ; except if the following line is encountered ^@@authors: FirstName1, LastName1 ; FirstName2, LastName2 ; etc.
-- date : default : date of last change except if the following line is encountered  ^@@date: 2021-09-10
-- source: Name of the root node except if the following line is encountered ^@@source:
-- By default, 1 framawrite node = 1 document.  Option for further developments: allow to give a level at generation for the split within framawrite node : :
-- 
-- par défaut: un doc == 1 NodeWrite
-- ## mean each ## section will be a new document with title the subsubsection title. Either it features options for author, date etc. or it will inherit the document's option.

sample =
  unlines
    [ "title1"
    , "title2"
    , "=="
    , "^@@authors: FirstName1, LastName1; FirstName2, LastName2"
    , "^@@date: 2021-09-10"
    , "^@@source: someSource"
    , "document contents 1"
    , "document contents 2"
    ]

parseSample = parse documentP "sample" (unpack sample)

data Author =
    Author { firstName :: Text
           , lastName :: Text }
    deriving (Show)
      
data Parsed =
  Parsed { title :: Text
         , authors :: [Author]
         , date :: Maybe Text
         , source :: Maybe Text
         , contents :: Text }
  deriving (Show)

documentP = do
  t <- titleP
  a <- optionMaybe authorsP
  d <- optionMaybe dateP
  s <- optionMaybe sourceP
  c <- contentsP
  pure $ Parsed { title = pack t
                , authors = fromMaybe [] a
                , date = pack <$> d
                , source = pack <$> s
                , contents = pack c }

titleDelimiterP = do
  newline
  string "=="
  tokenEnd
titleP :: Parser [Char]
titleP = manyTill anyChar (try titleDelimiterP)

authorsPrefixP = do
  _ <- string "^@@authors:"
  many (char ' ')
authorsP :: Parser [Author]
authorsP = try authorsPrefixP *> sepBy authorP (char ';')
authorP :: Parser Author
authorP = do
  fn <- manyTill anyChar (char ',')
  _ <- many (char ' ')
  --ln <- manyTill anyChar (void (char ';') <|> tokenEnd)
  ln <- manyTill anyChar (tokenEnd)
  pure $ Author { firstName = pack fn, lastName = pack ln }
  -- manyTill anyChar (void (char '\n') <|> eof)

datePrefixP = do
  _ <- string "^@@date:"
  many (char ' ')
dateP :: Parser [Char]
dateP = try datePrefixP
        *> manyTill anyChar tokenEnd

sourcePrefixP = do
  _ <- string "^@@source:"
  many (char ' ')
sourceP :: Parser [Char]
sourceP = try sourcePrefixP
          *> manyTill anyChar tokenEnd

contentsP = many anyChar

tokenEnd = void (char '\n') <|> eof
