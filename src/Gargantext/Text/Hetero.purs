module Gargantext.Text.Hetero where

import GHC.Real as R
import Data.Set as S
import Data.Map as M
import Data.List.Split as S
import Database.PostgreSQL.Simple as PGS
import Opaleye.PGTypes (PGInt4)
import Opaleye.Internal.Column (Column)

import Gargantext.Database.Gargandb
import Gargantext.Database.Private
--import Gargantext.Utils.Chronos

import Gargantext.Text.Words (cleanText)
import Gargantext.Text.Count (occurrences)

import Gargantext.Database.Simple

--main = do
--    t <- getTextquery
--    print (Prelude.map (heterogeinity . concat) $ S.chunksOf 3 t) 

-- heterogeinity sur concat texts
heterogeinity' :: Int -> Int -> Int -> IO [Integer]
heterogeinity' corpus_id limit x = do
    t <- getAbstract corpus_id limit
    Prelude.mapM (dicoStruct . occurrences) $ (S.chunksOf x) .  cleanText $ concat t

heterogeinity'' :: Int -> Int -> Int -> IO [Integer]
heterogeinity'' corpus_id limit size = do
    t <- getAbstract corpus_id limit
    Prelude.mapM (dicoStruct . occurrences) $ (S.chunksOf size) .  cleanText $ concat t


dicoStruct :: (Integral r, Monad m) => M.Map t r -> m r
dicoStruct dict_occ = do
    let keys_size = toInteger $ length $ M.keys dict_occ
    let total_occ = sum $ Prelude.map (\(x, y) -> y) $ M.toList dict_occ
    return $ div total_occ (fromIntegral keys_size)

-- heterogeinity sur UCT (Unité de Context Textuel)
heterogeinity :: [Char] -> IO Integer
heterogeinity string = do
    let dict_occ = occurrences $ cleanText string
    
    let keys_size = toInteger $ length $ M.keys dict_occ
    let total_occ = sum $ Prelude.map (\(x, y) -> y) $ M.toList dict_occ
    
    return $ div total_occ (fromIntegral keys_size)
    

--computeHeterogeinity
--  :: Fractional t =>
--       Opaleye.Internal.Column.Column Opaleye.PGTypes.PGInt4
--            -> IO (t, Integer, Integer)
computeHeterogeinity corpus_id = do
    c <- PGS.connect infoGargandb
    t <- getText c (nodeHyperdataText corpus_id)
    heterogeinity $ Prelude.concat t

main2 = do
    let corpus_ids = [
                ("ALL", 272927) -- 73
               ,("Histoire", 1387736) -- 28
               ,("Sciences Po", 1296892) -- 37
               ,("Phylosophie", 1170004) -- 20
               ,("Psychologie", 1345852) -- 37
               ,("Sociologie", 1246452)  -- 42
               ]
    
    r <- Prelude.map computeHeterogeinity $ Prelude.map (\(t,id) -> id) corpus_ids
    return r


