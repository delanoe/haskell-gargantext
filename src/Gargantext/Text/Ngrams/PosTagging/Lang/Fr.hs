{-|
Module      : Gargantext.Text.Ngrams.PosTagging.Lang.Fr
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

module Gargantext.Text.Ngrams.PosTagging.Lang.Fr (selectNgrams, groupNgrams, textTest)
    where

import Gargantext.Prelude
import Data.Text (Text)
import Data.Monoid ((<>))

selectNgrams :: [(Text, Text, Text)] -> [(Text, Text, Text)]
selectNgrams xs = filter selectNgrams' xs
    where
        selectNgrams' (_,"N"    ,_       ) = True
        selectNgrams' (_,"NC"   ,_       ) = True
        selectNgrams' (_,"NN+CC",_       ) = True
        selectNgrams' (_,_      ,"PERSON"      ) = True
        selectNgrams' (_,_      ,"ORGANIZATION") = True
        selectNgrams' (_,_      ,"LOCATION"    ) = True
        selectNgrams' (_,_      ,_       ) = False


groupNgrams :: [(Text, Text, Text)] -> [(Text, Text, Text)]
groupNgrams []       = []

--groupNgrams ((_,"DET",_):xs) = groupNgrams xs

-- "Groupe : nom commun et adjectifs avec conjonction"
groupNgrams ((n,"NC",n'):(j1,"ADJ",_):(_,"CC",_):(j2,"ADJ",_):xs) = groupNgrams (n1:n2:xs)
    where
        n1 = (n <> " " <> j1, "NC", n')
        n2 = (n <> " " <> j2, "NC", n')

-- /!\ sometimes N instead of NC (why?)
groupNgrams ((n,"N",n'):(j1,"ADJ",_):(_,"CC",_):(j2,"ADJ",_):xs) = groupNgrams (n1:n2:xs)
    where
        n1 = (n <> " " <> j1, "N", n')
        n2 = (n <> " " <> j2, "N", n')

-- Groupe : Adjectif + Conjonction de coordination + Adjectif
-- groupNgrams ((j1,"ADJ",_):(_,"CC",_):(j2,"ADJ",j2'):xs) = groupNgrams ((j1 <> " " <> j2, "ADJ", j2'):xs)

-- Groupe : Nom commun + préposition + Nom commun
groupNgrams ((n1,"NC",_):(p,"P",_):(n2,"NC",n2'):xs)  = groupNgrams ((n1 <> " " <> p <> " " <> n2, "NC", n2'):xs)
groupNgrams ((n1,"NC",_):(p,"P",_):(n2,"NPP",n2'):xs) = groupNgrams ((n1 <> " " <> p <> " " <> n2, "NC", n2'):xs)
groupNgrams ((n1,"NC",_):(prep,"P",_):(det,"DET",_):(n2,"NPP",n2'):xs) = groupNgrams ((n1 <> " " <> prep <> " " <> det <> " " <> n2, "NC", n2'):xs)

-- Groupe : Plusieurs adjectifs successifs
groupNgrams ((x,"ADJ",_):(y,"ADJ",yy):xs) = groupNgrams ((x <> " " <> y, "ADJ", yy):xs)

-- Groupe : nom commun et adjectif
groupNgrams ((x,"NC",_):(y,"ADJ",yy):xs)  = groupNgrams ((x <> " " <> y, "NC", yy):xs)
-- /!\ sometimes N instead of NC (why?)
groupNgrams ((x,"N",_):(y,"ADJ",yy):xs)   = groupNgrams ((x <> " " <> y, "NC", yy):xs)

-- Groupe : adjectif et nom commun
groupNgrams ((x,"ADJ",_):(y,"NC",yy):xs)  = groupNgrams ((x <> " " <> y, "NC", yy):xs)
-- /!\ sometimes N instead of NC (why?)
groupNgrams ((x,"ADJ",_):(y,"N",yy):xs)   = groupNgrams ((x <> " " <> y, "NC", yy):xs)


groupNgrams ((x,_,"PERSON"):(y,yy,"PERSON"):xs)             = groupNgrams ((x <> " " <> y,yy,"PERSON"):xs)
groupNgrams ((x,_,"ORGANIZATION"):(y,yy,"ORGANIZATION"):xs) = groupNgrams ((x <> " " <> y,yy,"ORGANIZATION"):xs)


-- Si aucune des règles précédentes n'est remplie
groupNgrams (x:xs)                        = (x:(groupNgrams xs))


textTest :: [Text]
textTest = [ "L'heure d'arrivée des coureurs dépend de la météo du jour."]

