{-|
Module      : Gargantext.Text.Ngrams.PosTagging.Lang.En
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Ngrams.PosTagging.Lang.En (group)
  where

--import Data.Text (Text)
import Data.Maybe (Maybe(Just))
import Gargantext.Text.Ngrams.PosTagging.CoreNLP
import Gargantext.Prelude

group :: [NgramsTag] -> [NgramsTag]
group [] = []
group ntags = group2 NP NP
           $ group2 NP VB
           $ group2 NP IN
           $ group2 IN DT
           $ group2 VB NP
           $ group2 JJ NP
           $ group2 JJ JJ
           $ group2 JJ CC
           $ ntags

------------------------------------------------------------------------
-- | FIXME p1 and p2 not really taken into account
group2 :: POS -> POS -> [NgramsTag] -> [NgramsTag]
group2 p1 p2 (x@(NgramsTag _ _ (Just p1') _):y@(NgramsTag _ _ (Just p2') _):z) =
  if (p1 == p1') && (p2 == p2')
     then group2 p1 p2 (x<>y : z)
     else (x : group2 p1 p2 (y:z))
group2 p1 p2 (x@(NgramsTag _ _ Nothing _):y) = (x: group2 p1 p2 y)
group2 _ _ [x@(NgramsTag _ _ (Just _) _)] = [x]
group2 p1 p2 (x@(NgramsTag _ _ (Just _) _):y@(NgramsTag _ _ Nothing _):z) = (x:y: group2 p1 p2 (y:z))
group2 _ _ [] = []

-- group3 :: POS -> POS -> POS -> [NgramsTag] -> [NgramsTag]
-- group xs = group3 NN IN DT xs


-- TO BE REMOVED old code
--groupNgrams ((j1,"JJ",j1'):(c1,"CC",c1'):(j2,"JJ",j2'):(j3,"JJ",_):xs)      = groupNgrams (jn1:cc:jn2:xs)
--    where
--        jn j' j'' jn' = (j' <> " " <> j'', "JJ", jn')
--        cc  = (c1, "CC", c1')
--        jn1 = (j1, "JJ", j1')
--        jn2 = jn j2 j3 j2'
--
--groupNgrams ((j1,"JJ",_):(_,"CC",_):(j2,"JJ",_):(n,"NN",nn):xs)      = groupNgrams (jn1:jn2:xs)
--    where
--        jn j m mm p = (j <> " " <> m, p, mm)
--        jn1 = jn j1 n nn ("NN+CC" :: Text)
--        jn2 = jn j2 n nn ("NN+CC" :: Text)
--
--groupNgrams ((j1,"JJ",_):(_,"CC",_):(j2,"JJ",_):(n,"NNS",nn):xs)      = groupNgrams (jn1:jn2:xs)
--    where
--        jn j m mm p = (j <> " " <> m, p, mm)
--        jn1 = jn j1 n nn ("NN+CC" :: Text)
--        jn2 = jn j2 n nn ("NN+CC" :: Text)
--
--groupNgrams ((x,"JJ",_):(y,"JJ",yy):xs)    = groupNgrams ((x <> " " <> y, "JJ", yy):xs)
--groupNgrams ((x,"JJ",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--groupNgrams ((x,"JJ",_):(y,"NNS",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--
--groupNgrams ((x,"NNP",_):(y,"NN",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--groupNgrams ((x,"NN",_):(y,"NP",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--
--groupNgrams ((x,"NN",_):(y,"NNS",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--groupNgrams ((x,"NP",_):(y,"NP",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--
--groupNgrams ((x,"NN",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)
--
--
---- extractNgrams "Test the antiinflammatory or analgesic activity?"
---- [[("``","``","O"),("Test","VB","O"),("the","DT","O"),("antiinflammatory activity analgesic activity","NN","O"),("?",".","O"),("''","''","O")]]
---- > should be (antiinflammatory activity) <> (analgesic activity)
--
--groupNgrams ((x,"NN",_):(o,"IN",_):(y,"NN",yy):xs)       = groupNgrams ((x <> " " <> o <> " " <> y, "NN", yy):xs)
--groupNgrams ((x,"NN",_):(o,"IN",_):(y,"NNP",yy):xs)      = groupNgrams ((x <> " " <> o <> " " <> y, "NN", yy):xs)
--
--groupNgrams ((x,"NN",_):(o,"IN",_):(det,"DT",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> o <> " " <> det <> " " <> y, "NN", yy):xs)
--groupNgrams ((x,"NN",_):(o,"IN",_):(det,"DT",_):(y,"NNP",yy):xs)   = groupNgrams ((x <> " " <> o <> " " <> det <> " " <> y, "NN", yy):xs)
--
--groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
--groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)
--groupNgrams ((x,_,"LOCATION"):(y,yy,"LOCATION"):xs)         = groupNgrams ((x <> " " <> y,yy,"LOCATION"):xs)
--
--groupNgrams (x:xs)                                          = (x:(groupNgrams xs))


--textTest :: [Text]
--textTest = [ "Alcoholic extract of Kaempferia galanga was tested for analgesic and antiinflammatory activities in animal models. "
--           , "Three doses, 300 mg/kg, 600 mg/kg and 1200 mg/kg of the plant extract prepared as a suspension in 2 ml of 2% gum acacia were used. "
--           , " Acute and sub acute inflammatory activities were studied in rats by carrageenan induced paw edema and cotton pellet induced granuloma models respectively. "
--          , "In both models, the standard drug used was aspirin 100 mg/kg. "
--          , "Two doses 600 mg/kg and 1200 mg/kg of plant extract exhibited significant (P<0.001) antiinflammatory activity in carrageenan model and cotton pellet granuloma model in comparison to control. "
--         , "Analgesic activity was studied in rats using hot plate and tail-flick models. "
--         , "Codeine 5 mg/kg and vehicle served as standard and control respectively. "
--         , "The two doses of plant extract exhibited significant analgesic activity in tail flick model (P<0.001) and hot plate model (P<0.001) in comparison to control. "
--         , "In conclusion K. galanga possesses antiinflammatory and analgesic activities. "]
