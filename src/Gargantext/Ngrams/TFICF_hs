module Data.Gargantext.Ngrams.TFICF where



data TFICF = TFICF { _tficfTerms    :: Text
                   , _tficfContext1 :: Context
                   , _tficfContext2 :: Context
                   , _tficfScore    :: Maybe Double
                   } deriving (Read, Show, Generics)


tfidf :: Text -> TFICF
tfidf txt = TFICF txt Document Corpus score
    where
        score = Nothing

