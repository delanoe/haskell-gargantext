
module Data.Gargantext.Ngrams.Words where
import Data.List (partition)
import Data.Set (fromList, notMember, member)
import Data.Char (isPunctuation, toLower, isAlpha, isSpace)

import NLP.Stemmer (stem, Stemmer(..))
import Language.Aspell (check, suggest, spellChecker, spellCheckerWithOptions)
import Language.Aspell.Options (ACOption(..))

--import Data.Either.Utils (fromRight)
import Data.ByteString.Internal (packChars)


get_lang x = do
    let lang = Lang (packChars x)
    spell_lang <- spellCheckerWithOptions [lang]
    return spell_lang

check' lang x = check lang (packChars x)
suggest' lang x = suggest lang (packChars x)

--spell_lang <- spellChecker
--lang = fromRight s
--suggest' lang x

-- stem French "naturelles"


-- paragraphes
-- lines
-- sentences

-- Prelude.map (\x -> stem French x) $ cleanText "Les hirondelles s envolent dans les cieux."
repl :: Char -> Char
repl x
    | x == '\'' = ' '
    | x == '/' = ' '
    -- | x == '\t' = ' '
    -- | x == '\n' = ' '
    | otherwise = x

cleanText text = do
    -- pb avec \'
    --words $ filter (not . isPunctuation) $ Prelude.map toLower text
    words $ filter (\x -> isAlpha x || isSpace x) $ Prelude.map (repl . toLower) text

isMiamWord word = do
    let miamWord_set = fromList ["salut", "phrase"]
    member word miamWord_set

isStopWord word = do
    let stopWord_set = fromList ["de", "la", "une", "avec"]
    member word stopWord_set

wordsMain = do
    let text = "Salut, ceci est une phrase \n\n avec de la ponctuation !"
    print $ partition (not . isStopWord) $ cleanText text
    print $ filter (not . isStopWord) $ cleanText text
    --print $ filter isStopWord $ words $ filter (not . isPunctuation) text
