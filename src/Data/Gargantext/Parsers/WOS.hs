{-# LANGUAGE OverloadedStrings #-}

module Data.Gargantext.Parsers.WOS where

-- TOFIX : Should import Data.Gargantext.Prelude here
import Prelude hiding (takeWhile, take, concat, readFile)

import qualified Data.List as DL
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Control.Applicative


import Data.Gargantext.Types


-- | wosParser parses ISI format from
-- Web Of Science Database
wosParser :: ByteString -> IO Corpus
wosParser = undefined


wosParser' :: Parser [Maybe [ByteString]]
wosParser' = do
    -- TODO Warning if version /= 1.0
    -- FIXME anyChar (string ..) /= exact string "\nVR 1.0" ?
    _ <- manyTill anyChar (string $ pack "\nVR 1.0")
    ns <- many1 wosNotice <* (string $ pack "\nEF")
    return ns

wosNotice :: Parser (Maybe [ByteString])
wosNotice = startNotice *> wosFields <* endNotice
    where
      endNotice :: Parser [Char]
      endNotice = manyTill anyChar (string $ pack "\nER\n")

      startNotice :: Parser ByteString
      startNotice = "\nPT " *> takeTill isEndOfLine

field' :: Parser (ByteString, [ByteString])
field' = do
    f  <- "\n" *> take 2 <* " "
    a  <- takeTill isEndOfLine
    as <- try wosLines
    let as' = case DL.length as > 0 of
            True  -> as
            False -> []
    return (f, [a] ++ as')

wosFields' :: Parser [(ByteString, [ByteString])]
wosFields' = many field'

wosFields :: Parser (Maybe [ByteString])
wosFields = do
--    a <- field "AU"
--    t <- field "TI"
--    s <- field "SO"
--    d <- field "DI" -- DOI
--    p <- field "PD"
--    b <- field "AB"
--    u <- field "UT"
    ws <- many field'
    return $ DL.lookup "UT" ws
--    return $ HyperdataDocument 
--                    Just "WOS"
--                    DL.lookup "DI" ws
--                    DL.lookup "URL" ws
--                    DL.lookup "PA" ws
--                    DL.lookup "TI" ws


wosLines :: Parser [ByteString]
wosLines = many line
    where
        line :: Parser ByteString
        line = "\n  " *> takeTill isEndOfLine

