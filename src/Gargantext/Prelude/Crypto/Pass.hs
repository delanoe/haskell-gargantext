{-|
Module      : Gargantext.Prelude.Crypto.Pass
Description :
Copyright   : (c) CNRS, 2017-Present
License     : Public Domain
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

To avoid weak password, just offer an easy way to make "good" one and
let user add his own entropy.

Thanks to 
https://zuttobenkyou.wordpress.com/2011/12/23/simple-password-generation-with-haskell/

-}


module Gargantext.Prelude.Crypto.Pass
      where

-- import Data.List (nub)
-- import System.Environment (getArgs)
-- import System.IO (hSetEcho)
import Control.Monad.State
import Crypto.Random (cprgGenerate)
import Crypto.Random.AESCtr
import Data.Binary (decode)
import Prelude
import qualified Data.ByteString.Lazy as B


keysChar, keysNum, keysPunc, keysCharNum, keysAll, keysHex :: String
keysChar = ['a'..'z'] ++ ['A'..'Z']
keysHex = ['a'..'f']
keysNum = ['0'..'9']
keysPunc = "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/? "
keysCharNum = keysChar ++ keysNum
keysAll = keysChar ++ keysNum ++ keysPunc
 
giveKey ::  String -> Char -> Int -> Char
giveKey keysCustom c n = extractChar $ case c of
    'i'  -> (keysNum ++ keysHex)
    'j'  -> keysNum
    'k'  -> keysChar
    'l'  -> keysCharNum
    ';'  -> keysPunc
    'h'  -> (keysCharNum ++ keysCustom)
    '\n' -> ['\n']
    _    -> keysAll
    where
    extractChar xs = xs!!mod n (length xs)
 
showRandomKey :: Int -> String -> StateT AESRNG IO ()
showRandomKey len keysCustom = handleKey =<< liftIO getChar
    where
    handleKey key = case key of
        '\n' -> liftIO (putChar '\n') >> showRandomKey len keysCustom
        'q' -> (liftIO $ putStrLn "\nBye!") >> return ()
        _ -> mapM_ f [0..len] >> (liftIO $ putStrLn []) >> showRandomKey len keysCustom
        where
        f _ = liftIO
            . putChar
            . giveKey keysCustom key
            . (\n -> mod n (length (keysAll ++ keysCustom) - 1))
            =<< aesRandomInt
 
aesRandomInt :: StateT AESRNG IO Int
aesRandomInt = do
    aesState <- get
    -- aesState <- liftIO makeSystem
    -- let aesState = 128
    let (bs, aesState') = cprgGenerate 64 aesState
    put aesState'
    return (decode $ B.fromChunks [bs])

gargPass :: IO (Int, AESRNG)
gargPass = do
  -- let as = ["alphanumeric","punctuation"]
  -- let as' = filter (\c -> elem c keysAll) . nub $ unwords as
  aesState <- makeSystem -- gather entropy from the system to use as the initial seed
  --_ <- runStateT (showRandomKey len as') aesState -- enter loop
  -- return ()
  pass <- runStateT aesRandomInt aesState -- enter loop
  pure pass

{- 
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering  -- disable buffering from STDIN
    hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
    hSetEcho stdin False             -- disable terminal echo
    as <- getArgs
    let as' = filter (\c -> elem c keysAll) . nub $ unwords as
    mapM_ putStrLn
        [ []
        , "poke: 'q'     quit"
        , "      'j'     number"
        , "      'k'     letter"
        , "      'l'     alphanumeric"
        , "      ';'     punctuation"
        , "      'h'     alphanumeric" ++ (if null as' then [] else " + " ++ as')
        , "      'i'     hexadecimal"
        , "      'ENTER' newline"
        , "      else    any"
        , []
        ]
    aesState <- makeSystem -- gather entropy from the system to use as the initial seed
    _ <- runStateT (showRandomKey as') aesState -- enter loop
    return ()
-}
