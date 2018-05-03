{-|
Module      : Gargantext.Text.Lang.En
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

module Gargantext.Text.Lang.En (selectNgrams, groupNgrams, textTest) where

import Gargantext.Prelude
import Data.Text (Text)
import Data.Monoid ((<>))

selectNgrams :: [(Text, Text, Text)] -> [(Text, Text, Text)]
selectNgrams xs = filter isNgrams xs
    where
        isNgrams (_,"NN"   ,_             ) = True
        isNgrams (_,"NNS"  ,_             ) = True
        isNgrams (_,"NNP"  ,_             ) = True
        isNgrams (_,"NN+CC",_             ) = True
        isNgrams (_,_      ,"PERSON"      ) = True
        isNgrams (_,_      ,"ORGANIZATION") = True
        isNgrams (_,_      ,"LOCATION"    ) = True
        isNgrams (_,_      ,_             ) = False


groupNgrams :: [(Text, Text, Text)] -> [(Text, Text, Text)]
groupNgrams []       = []

groupNgrams ((j1,"JJ",j1'):(c1,"CC",c1'):(j2,"JJ",j2'):(j3,"JJ",_):xs)      = groupNgrams (jn1:cc:jn2:xs)
    where
        jn j' j'' jn' = (j' <> " " <> j'', "JJ", jn')
        cc  = (c1, "CC", c1')
        jn1 = (j1, "JJ", j1')
        jn2 = jn j2 j3 j2'

groupNgrams ((j1,"JJ",_):(_,"CC",_):(j2,"JJ",_):(n,"NN",nn):xs)      = groupNgrams (jn1:jn2:xs)
    where
        jn j m mm p = (j <> " " <> m, p, mm)
        jn1 = jn j1 n nn ("NN+CC" :: Text)
        jn2 = jn j2 n nn ("NN+CC" :: Text)

groupNgrams ((j1,"JJ",_):(_,"CC",_):(j2,"JJ",_):(n,"NNS",nn):xs)      = groupNgrams (jn1:jn2:xs)
    where
        jn j m mm p = (j <> " " <> m, p, mm)
        jn1 = jn j1 n nn ("NN+CC" :: Text)
        jn2 = jn j2 n nn ("NN+CC" :: Text)

groupNgrams ((x,"JJ",_):(y,"JJ",yy):xs)    = groupNgrams ((x <> " " <> y, "JJ", yy):xs)
groupNgrams ((x,"JJ",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)
groupNgrams ((x,"JJ",_):(y,"NNS",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)

groupNgrams ((x,"NNP",_):(y,"NN",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)
groupNgrams ((x,"NN",_):(y,"NP",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)

groupNgrams ((x,"NN",_):(y,"NNS",yy):xs)   = groupNgrams ((x <> " " <> y, "NN", yy):xs)
groupNgrams ((x,"NP",_):(y,"NP",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)

groupNgrams ((x,"NN",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> y, "NN", yy):xs)


-- extractNgrams "Test the antiinflammatory or analgesic activity?"
-- [[("``","``","O"),("Test","VB","O"),("the","DT","O"),("antiinflammatory activity analgesic activity","NN","O"),("?",".","O"),("''","''","O")]]
-- > should be (antiinflammatory activity) <> (analgesic activity)

groupNgrams ((x,"NN",_):(o,"IN",_):(y,"NN",yy):xs)       = groupNgrams ((x <> " " <> o <> " " <> y, "NN", yy):xs)
groupNgrams ((x,"NN",_):(o,"IN",_):(y,"NNP",yy):xs)      = groupNgrams ((x <> " " <> o <> " " <> y, "NN", yy):xs)

groupNgrams ((x,"NN",_):(o,"IN",_):(det,"DT",_):(y,"NN",yy):xs)    = groupNgrams ((x <> " " <> o <> " " <> det <> " " <> y, "NN", yy):xs)
groupNgrams ((x,"NN",_):(o,"IN",_):(det,"DT",_):(y,"NNP",yy):xs)   = groupNgrams ((x <> " " <> o <> " " <> det <> " " <> y, "NN", yy):xs)

groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)
groupNgrams ((x,_,"LOCATION"):(y,yy,"LOCATION"):xs)         = groupNgrams ((x <> " " <> y,yy,"LOCATION"):xs)

groupNgrams (x:xs)                                          = (x:(groupNgrams xs))


textTest :: [Text]
textTest = [ "Alcoholic extract of Kaempferia galanga was tested for analgesic and antiinflammatory activities in animal models. "
           , "Three doses, 300 mg/kg, 600 mg/kg and 1200 mg/kg of the plant extract prepared as a suspension in 2 ml of 2% gum acacia were used. "
           , " Acute and sub acute inflammatory activities were studied in rats by carrageenan induced paw edema and cotton pellet induced granuloma models respectively. "
          , "In both models, the standard drug used was aspirin 100 mg/kg. "
          , "Two doses 600 mg/kg and 1200 mg/kg of plant extract exhibited significant (P<0.001) antiinflammatory activity in carrageenan model and cotton pellet granuloma model in comparison to control. "
         , "Analgesic activity was studied in rats using hot plate and tail-flick models. "
         , "Codeine 5 mg/kg and vehicle served as standard and control respectively. "
         , "The two doses of plant extract exhibited significant analgesic activity in tail flick model (P<0.001) and hot plate model (P<0.001) in comparison to control. "
         , "In conclusion K. galanga possesses antiinflammatory and analgesic activities. "]


