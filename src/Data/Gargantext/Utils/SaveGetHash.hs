
module Data.Gargantext.Utils.SaveGetHash where

import System.FilePath (addExtension, joinPath)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.List (elem, intersperse, insert)
import Data.List.Extra (chunksOf)
import Data.Digest.Pure.MD5 (md5)
import System.Directory (getDirectoryContents, createDirectory, findFile, createDirectoryIfMissing)
import Control.Monad (foldM)

import Data.List (splitAt)
import Data.ByteString.Lazy.Internal (packChars)
import qualified Data.ByteString.Lazy as BL 

import Codec.Compression.Zlib (compress, decompress)

data Config = Config {
      root          :: String
    , chunkSize     :: Int
    , compression   :: Bool
    } deriving Show

conf = Config {
      root="/tmp/robot"
    , chunkSize=2
    , compression = True
              }

chunkUrl :: Int -> ByteString -> [[Char]]
chunkUrl a url = chunksOf a $ show $ md5 url

-- replace it with createDirectoryIfMissing
existOrCreate :: [[Char]] -> FilePath -> IO [[Char]]
existOrCreate path_ dir = do
    let path = joinPath path_
    let returnPath = return $ path_ ++ [dir]
    
    is <- elem dir <$> getDirectoryContents path  -- ?
    case is of
      True  -> do
          returnPath
      False -> do
          createDirectory $ path ++ "/" ++ dir
          returnPath

doPath :: [[Char]] -> [FilePath] -> IO [[Char]]
doPath root path = foldM (\x y -> existOrCreate x y) root path


splitAt' :: Int -> Int -> [Char] -> ([Char], [Char], [Char])
splitAt' i1 i2 x = (a, b, c) where
    (a, a') = splitAt i1 x
    (b, c)  = splitAt i2 a'


-- ne pas écraser le fichier s'il existe
-- spliter l'url proprement
saveFile :: ByteString -> String -> IO String
saveFile url'' file = do
    let url' = chunkUrl (chunkSize conf) url''
    let url = init url'
    
    -- add extension according to the filetype
    let filename = Prelude.foldl addExtension (last url') ["html", "zlib"]
    
    doPath [(root conf)] url
    
    let path = (root conf) ++ "/" ++ joinPath url ++ "/" ++ filename
    
    --case (findFile ["/tmp/sdfs"] "file.hmtl.zib"
    -- Nothing -> create
    -- _ -> change name
    case (compression conf) of
      True  -> BL.writeFile path (compress $ packChars file)
      False -> writeFile path file
    return path


getFile :: FilePath -> IO ByteString
getFile path = do
    case (compression conf) of
      True  -> decompress <$> BL.readFile path
      False -> packChars  <$> Prelude.readFile path


-- resources
-- add Resource


-- levensthein distance...
