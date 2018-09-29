{-|
Module      : Gargantext.Text.Terms.Stop
Description : Mono Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Stop words and (how to learn it).

Main type here is String.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Stop
  where

import Numeric.Probability.Distribution ((??))
import qualified Numeric.Probability.Distribution as D

import Data.Char (toLower)
import qualified Data.List as DL

import Data.Maybe (maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM

import Data.String (String)

import Data.Text (pack, unpack)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Text.Terms.Mono (words)
import Gargantext.Text.Metrics.Count (occurrencesWith)

import Gargantext.Text.Samples.FR as FR
import Gargantext.Text.Samples.EN as EN
import Gargantext.Text.Samples.DE as DE
import Gargantext.Text.Samples.SP as SP
import Gargantext.Text.Samples.CH as CH

------------------------------------------------------------------------
data Candidate = Candidate { stop :: Double
                           , noStop :: Double
 } deriving (Show)

-- * String preparation

-- | String prepare
blanks :: String -> String
blanks [] = []
blanks xs = [' '] <> xs <> [' ']

-- | Blocks increase the size of the word to ease computations
-- some border and unexepected effects can happen, need to be tested
blockOf :: Int -> String -> String
blockOf n st = DL.concat $ DL.take n $ DL.repeat st

-- | Chunks is the same function as splitBy in Context but for Strings,
-- not Text (without pack and unpack operations that are not needed).
chunks :: Int -> Int -> String -> [String]
chunks n m = DL.take m . filter (not . all (== ' ')) . chunkAlong (n+1) 1 . DL.concat . DL.take 1000 . DL.repeat . blanks

allChunks :: [Int] -> Int -> String -> [String]
allChunks ns m st = DL.concat $ map (\n -> chunks n m st) ns

allChunks' :: [Int] -> Int -> String -> [[String]]
allChunks' ns m st = map (\n -> chunks n m st) ns

------------------------------------------------------------------------
-- * Analyze candidate
type StringSize = Int
type TotalFreq  = Int
type Freq       = Int
type Word       = String

data LangWord = LangWord Lang Word

type LangProba = Map Lang Double

------------------------------------------------------------------------


detectLangs :: String -> LangProba
detectLangs s = detect (wordsToBook [0..2] s) testEL

testEL :: EventLang
testEL = toEventLangs [0..2] [ LangWord EN EN.textMining
                              , LangWord FR FR.textMining
                              , LangWord DE DE.textMining
                              , LangWord SP SP.textMining
                              , LangWord CH CH.textMining
                              ]

detect :: EventBook -> EventLang -> LangProba
detect (EventBook mapFreq _) el = DM.unionsWith (+) $ map (\(s,n) -> DM.map (\eb -> (fromIntegral n) * peb s eb) el) $ filter (\x -> fst x /= "  ") $ DM.toList mapFreq

------------------------------------------------------------------------
-- | TODO: monoids
type EventLang = Map Lang EventBook
toEventLangs :: [Int] -> [LangWord] -> EventLang
toEventLangs ns = foldl' (opLang (+)) (emptyEventLang ns) . map (toLang ns)

emptyEventLang :: [Int] -> EventLang
emptyEventLang ns = toLang ns (LangWord FR "")

toLang :: [Int] -> LangWord -> EventLang
toLang ns (LangWord l txt) = DM.fromList [(l, wordsToBook ns txt)]

opLang :: (Freq -> Freq -> Freq) -> EventLang -> EventLang -> EventLang
opLang f = DM.unionWith (op f)

------------------------------------------------------------------------
-- | TODO: monoids (but proba >= 0)

peb :: String -> EventBook -> Double
peb st (EventBook mapFreq mapN) = (fromIntegral a) / (fromIntegral b)
  where
    a = maybe 0 identity $ DM.lookup st mapFreq
    b = maybe 1 identity $ DM.lookup (length st) mapN

data EventBook = EventBook { events_freq :: Map String     Freq
                           , events_n    :: Map StringSize TotalFreq
                           }
                             deriving (Show)

emptyEventBook :: [Int] -> EventBook
emptyEventBook ns = wordToBook ns " "

wordsToBook :: [Int] -> String -> EventBook
wordsToBook ns txt = foldl' (op (+)) (emptyEventBook ns) eventsBook
  where
    ws = map unpack $ words $ pack txt
    eventsBook = map (wordToBook ns) ws

wordToBook :: [Int] -> Word -> EventBook
wordToBook ns txt = EventBook ef en
  where
    chks = allChunks' ns 10 txt
    en = DM.fromList $ map (\(n,ns') -> (n, length ns')) $ zip ns chks
    ef = foldl' DM.union DM.empty $ map (occurrencesWith identity) chks

op :: (Freq -> Freq -> Freq) -> EventBook -> EventBook -> EventBook
op f (EventBook ef1 en1)
     (EventBook ef2 en2) = EventBook (DM.unionWith f ef1 ef2)
                                     (DM.unionWith f en1 en2)


------------------------------------------------------------------------
------------------------------------------------------------------------
-- * Make the distributions
makeDist :: [String] -> D.T Double String
makeDist = D.uniform . DL.concat . map (allChunks [0,2] 10)

stopDist :: D.T Double String
stopDist = makeDist stopList

candDist :: D.T Double String
candDist = makeDist candList

------------------------------------------------------------------------
sumProba :: Num a => D.T a String -> [Char] -> a
sumProba ds x = sum $ map ((~?) ds) $ allChunks [0,2] 10 $ map toLower x

-- | Get probability according a distribution
(~?) :: (Num prob, Eq a) => D.T prob a -> a -> prob
(~?) ds x = (==x) ?? ds

------------------------------------------------------------------------
candidate :: [Char] -> Candidate
candidate x = Candidate (sumProba stopDist x) (sumProba candDist x)

------------------------------------------------------------------------
candList :: [String]
candList = [ "france", "alexandre", "mael", "constitution"
           , "etats-unis", "associes", "car", "train", "spam"]


stopList :: [String]
stopList = map show ([0..9]::[Int]) <> [
    "a","a's","able","about","above","apply","according","accordingly",
    "across","actually","after","afterwards","again","against",
    "ain't","all","allow","allows","almost","alone","along",
    "involves", "already","also","although","always","am","among","amongst",
    "an","and","another","any","anybody","anyhow","anyone","anything",
    "anyway","anyways","anywhere","analyze","apart","appear","appreciate","appropriate",
    "are","aren't","around","as","aside","ask","asking","associated","at",
    "available","away","awfully","based", "b","be","became","because","become",
    "becomes","becoming","been","before","beforehand","behind","being",
    "believe","below","beside","besides","best","better","between","beyond",
    "both","brief","but","by","c","c'mon","c's","came","can","can't","cannot",
    "cant","cause","causes","certain","certainly","changes","clearly","co",
    "com","come","comes","common","concerning","consequently","consider","considering",
    "contain","containing","contains","corresponding","could","couldn't","course",
    "currently","d","definitely","described","detects","detecting","despite","did","didn't","different",
    "do","does","doesn't","doing","don't","done","down","downwards","during","e",
    "each","edu","eg","eight","either","else","elsewhere","enough","entirely",
    "especially","et","etc","even","ever","every","everybody","everyone",
    "everything","everywhere","ex","exactly","example","except","f","far",
    "few","find","fifth","first","five","followed","following","follows","for",
    "former","formerly","forth","four","from","further","furthermore","g",
    "get","gets","getting","given","gives","go","goes","going","gone","got",
    "gotten","greetings","h","had","hadn't","happens","hardly","has","hasn't",
    "have","haven't","having","he","he's","hello","help","hence","her","here",
    "here's","hereafter","hereby","herein","hereupon","hers","herself","hi",
    "him","himself","his","hither","hopefully","how","howbeit","however","i",
    "i'd","identify","i'll","i'm","i've","ie","if","ignored","immediate","in","inasmuch",
    "inc","indeed","indicate","indicated","indicates","inner","insofar",
    "instead","into","inward","is","isn't","it","it'd","it'll","it's","its",
    "itself","j","just","k","keep","keeps","kept","know","known","knows","l",
    "last","lately","later","latter","latterly","least","less","lest","let",
    "let's","like","liked","likely","little","look","looking","looks","ltd",
    "m","mainly","many","may","maybe","me","mean","meanwhile","merely","might",
    "more","moreover","most","mostly","much","must","my","myself","n",
    "name","namely","nd","near","nearly","necessary","need","needs","neither",
    "never","nevertheless","new","next","nine","no","nobody","non","none",
    "noone","nor","normally","not","nothing","novel","now","nowhere","o",
    "obviously","of","off","often","oh","ok","okay","old","on","once","one",
    "ones","only","onto","or","other","others","otherwise","ought","our",
    "ours","ourselves","out","outside","over","overall","own","p","particular",
    "particularly","per","perhaps","placed","please","plus","possible",
    "presents","presumably","probably","provides","q","que","quite","qv","r","rather",
    "rd","re","really","reasonably","regarding","regardless","regards",
    "relatively","respectively","right","s","said","same","saw","say",
    "saying","says","second","secondly","see","seeing","seem","seemed",
    "seeming","seems","seen","self","selves","sensible","sent","serious",
    "seriously","seven","several","shall","she","should","shouldn't","since",
    "six","so","some","somebody","somehow","someone","something","sometime",
    "sometimes","somewhat","somewhere","soon","sorry","specified","specify",
    "specifying","still","sub","such","sup","sure","t","t's","take","taken",
    "tell","tends","th","than","thank","thanks","thanx","that","that's",
    "thats","the","their","theirs","them","themselves","then","thence","there",
    "there's","thereafter","thereby","therefore","therein","theres",
    "thereupon","these","they","they'd","they'll","they're","they've",
    "think","third","this","thorough","thoroughly","those","though","three",
    "through","throughout","thru","thus","to","together","too","took","toward",
    "towards","tried","tries","truly","try","trying","twice","two","u","un",
    "under","unfortunately","unless","unlikely","until","unto","up","upon",
    "us","use","used","useful","uses","using","usually","uucp","v","value",
    "various","very","via","viz","vs","w","want","wants","was","wasn't","way",
    "we","we'd","we'll","we're","we've","welcome","well","went","were",
    "weren't","what","what's","whatever","when","whence","whenever","where",
    "where's","whereafter","whereas","whereby","wherein","whereupon",
    "wherever","whether","which","while","whither","who","who's","whoever",
    "whole","whom","whose","why","will","willing","wish","with","within",
    "without","won't","wonder","would","wouldn't","x","y","yes","yet","you",
    "you'd","you'll","you're","you've","your","yours","yourself","yourselves",
    "z","zero"]




