module Data.Gargantext.Parsers.WOS where

import Prelude hiding (takeWhile, take, concat, readFile)
import qualified Data.List as DL
import Data.Map as DM
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (anyChar, char8, endOfLine, isDigit_w8, isAlpha_ascii, isEndOfLine)
import Data.ByteString (ByteString, unpack, pack, concat, readFile)

import Data.Either.Extra(Either(..))
import Control.Applicative

import Control.Monad (join)

-- To be removed just for Tests
--
-- import Codec.Archive.LibZip (withArchive, fileNames, sourceFile, addFile)
--import Codec.Archive.LibZip.Types (ZipSource, OpenFlag (CreateFlag))

import Control.Concurrent.Async as CCA (mapConcurrently)

import           System.Environment

import Codec.Archive.Zip
import Path (parseAbsFile)
import Path.IO (resolveFile')
-- import qualified Data.ByteString.Lazy as B
import Control.Applicative ( (<$>) )


zipFiles :: FilePath -> IO [ByteString]
zipFiles fp = do
    path    <- resolveFile' fp
    entries <- withArchive path (DM.keys <$> getEntries)
    bs      <- mapConcurrently (\s -> withArchive path (getEntry s)) entries
    pure bs


parseFile :: ParserType -> ByteString -> IO Int
parseFile p x = case runParser p x of
        Left  e -> pure 1
        Right r -> pure $ length r

testWos :: FilePath -> IO [Int]
testWos fp = join $ mapConcurrently (parseFile WOS) <$> zipFiles fp

-- type Parser a = a -> Text -> [Document]
data ParserType = WOS | CSV

wosParser :: Parser [Maybe [ByteString]]
wosParser = do
    -- TODO Warning if version /= 1.0
    _ <- manyTill anyChar (string "\nVR 1.0")
    ns <- many1 wosNotice <* "\nEF"
    return ns

startNotice :: Parser ByteString
startNotice = "\nPT " *> takeTill isEndOfLine

wosNotice :: Parser (Maybe [ByteString])
wosNotice = do
    n <- startNotice *> wosFields <* manyTill anyChar (string "\nER\n")
    return n

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

runParser :: ParserType -> ByteString -> Either String [Maybe [ByteString]]
runParser p x = parseOnly parser x
    where
        parser = case p of 
                  WOS -> wosParser
                  _   -> error "Not implemented yet"

-- isTokenChar :: Word8 -> Bool
-- isTokenChar = inClass "!#$%&'()*+./0-9:<=>?@a-zA-Z[]^_`{|}~-\n"

