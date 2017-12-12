{-# LANGUAGE OverloadedStrings #-}

module Data.Gargantext.Parsers.WOS where

import Prelude hiding (takeWhile, take, concat, readFile)
import qualified Data.List as DL
import Data.Map as DM
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Data.Either.Extra(Either(..))
import Control.Applicative

import Control.Monad (join)

-- To be removed just for Tests
--
-- import Codec.Archive.LibZip (withArchive, fileNames, sourceFile, addFile)
--import Codec.Archive.LibZip.Types (ZipSource, OpenFlag (CreateFlag))

import Control.Concurrent.Async as CCA (mapConcurrently)

import Codec.Archive.Zip
import Path.IO (resolveFile')
-- import qualified Data.ByteString.Lazy as B
import Control.Applicative ( (<$>) )

-- type Parser a = a -> Text -> [Document]
data ParserType = WOS | CSV

type WosDoc = ByteString


wosParser :: Parser [Maybe [WosDoc]]
wosParser = do
    -- TODO Warning if version /= 1.0
    -- FIXME anyChar (string ..) /= exact string "\nVR 1.0" ?
    _ <- manyTill anyChar (string $ pack "\nVR 1.0")
    ns <- many1 wosNotice <* (string $ pack "\nEF")
    return ns

wosNotice :: Parser (Maybe [WosDoc])
wosNotice = startNotice *> wosFields <* endNotice

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
--

wosLines :: Parser [ByteString]
wosLines = many line
    where
        line :: Parser ByteString
        line = "\n  " *> takeTill isEndOfLine

runParser :: ParserType -> ByteString -> Either String [Maybe [WosDoc]]
runParser p x = parseOnly parser x
    where
        parser = case p of 
                  WOS -> wosParser
                  _   -> error "Not implemented yet"

-- isTokenChar :: Word8 -> Bool
-- isTokenChar = inClass "!#$%&'()*+./0-9:<=>?@a-zA-Z[]^_`{|}~-\n"


zipFiles :: FilePath -> IO [ByteString]
zipFiles fp = do
    path    <- resolveFile' fp
    entries <- withArchive path (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive path (getEntry s)) entries
    pure bs


parseFile :: ParserType -> ByteString -> IO Int
parseFile p x = case runParser p x of
        Left  _ -> pure 0
        Right r -> pure $ length r

parseWos :: FilePath -> IO [Int]
parseWos fp = join $ mapConcurrently (parseFile WOS) <$> zipFiles fp



