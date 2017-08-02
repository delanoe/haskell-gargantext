

-- DEFINITIONS as SPECS 
-- (Engineering axioms for Gargantext)


------------------------------------------------------------------------
-- From file to corpus
------------------------------------------------------------------------

-- > A Corpus is a list of Documents
data Corpus   = [Document]

-- > A Document should have a date, some text and a maybe a language.
-- > Remarks : 
--         > If no date then force one ?
--         > Analyze either text or numbers
--         > only one language per document
data Document = Document { date     :: UTCTime
                         , uce      :: Map Text $ Either (Maybe Text) (Maybe Double)
                         , lang     :: Maybe Language
                         }

parseFiles :: Maybe ParserType -> [File] -> Corpus
parseFiles = undefined

-- This function exists already (in Python)
parseFile' :: ParserType -> File -> Maybe [Document]
parseFile' = undefined

-- This function does not exist yet 
parseFile :: Maybe ParserType -> File -> Maybe [Document]
parseFile parserType file = documents
    where
        documents = case parserType of
                      
                      Nothing           -> case guessParserType file of
                                            Nothing          -> askUser "Answer to the question with link to $doc"
                                            Just parserType' -> parseFile (Just parserType') file
                      
                      Just parserType'' -> case parserType''         of
                                            UnsupportedYet   -> askUser "Not supported yet, which priority ?"
                                            otherwise        -> parseFile' parserType'' file

data ParserType = RIS | ISI | XML | CSV | Europresse | Book | UnsupportedYet
guessParserType :: File -> Maybe ParserType
guessParserType = undefined


------------------------------------------------------------------------
-- What kind of interactions with our users ?
------------------------------------------------------------------------

-- Question is Text only
type Question = Text

-- Possible Answers:
data Answer = ClosedAnswer | NumAnswer | OpenAnswer
-- Definitions of the Answers
type ClosedAnswer   = Bool
type OpenAnswer     = Text
type NumAnswer      = Int
-- Un formulaire est un mapping entre question et peut-être une réponse
-- Un formulaire vide a Nothing au champs (Maybe Answer)
-- Une question répondue a la valeur (Just Response)
type Formular = Map Question (Maybe Answer)

askUser :: Question -> ClosedAnswer
askUser = undefined

data Advice = BugReport | WishList
askUser' :: Question -> Advice
askUser' question = case askUser question of
                      True  -> BugReport
                      False -> WishList


------------------------------------------------------------------------
-- Specs for Lang Detection
------------------------------------------------------------------------
data Language   = English | French 

tagDoc :: Document -> Ngrams
tagDoc doc = ngrams
    where
        ngrams = case lang doc of
                   Nothing -> case guessLang doc of
                                Nothing -> tag


------------------------------------------------------------------------
-- Specs for ngrams Worflow
------------------------------------------------------------------------
