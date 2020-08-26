{-|
Module      : Gargantext.Core.Text.Samples.EN
Description : Sample of English Text
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Source: Wikipedia
Page  : text mining

-}



module Gargantext.Core.Text.Samples.EN where

import Data.String (String)

textSample :: String
textSample = "Text mining, also referred to as text data mining, roughly equivalent to text analytics, is the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning. Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interestingness. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities). Text analysis involves information retrieval, lexical analysis to study word frequency distributions, pattern recognition, tagging/annotation, information extraction, data mining techniques including link and association analysis, visualization, and predictive analytics. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods. A typical application is to scan a set of documents written in a natural language and either model the document set for predictive classification purposes or populate a database or search index with the information extracted."

stopList :: [String]
stopList = 
  ["a", "a's", "able", "about", "above", "according", "accordingly"
  , "across", "actually", "after", "afterwards", "again", "against"
  , "ain't", "all", "allow", "allows", "almost", "alone", "along"
  , "already", "also", "although", "always", "am", "among", "amongst", "an"
  , "analyze", "and", "another", "any", "anybody", "anyhow", "anyone"
  , "anything", "anyway", "anyways", "anywhere", "apart", "appear"
  , "apply", "appreciate", "appropriate", "are", "aren't", "around"
  , "as", "aside", "ask", "asking", "associated", "at", "available"
  , "away", "awfully", "b", "based", "be", "became", "because", "become"
  , "becomes", "becoming", "been", "before", "beforehand", "behind"
  , "being", "believe", "below", "beside", "besides", "best", "better"
  , "between", "beyond", "both", "brief", "but", "by", "c", "c'mon", "c's"
  , "came", "can", "can't", "cannot", "cant", "cause", "causes", "certain"
  , "certainly", "changes", "clearly", "co", "com", "come", "comes"
  , "common", "concerning", "consequently", "consider", "considering"
  , "contain", "containing", "contains", "corresponding", "could"
  , "couldn't", "course", "currently", "d", "definitely", "described"
  , "despite", "detecting", "detects", "did", "didn't", "different", "do"
  , "does", "doesn't", "doing", "don't", "done", "down", "downwards"
  , "during", "e", "each", "edu", "eg", "eight", "either", "else"
  , "elsewhere", "enough", "entirely", "especially", "et", "etc", "even"
  , "ever", "every", "everybody", "everyone", "everything", "everywhere"
  , "ex", "exactly", "example", "except", "f", "far", "few", "fifth"
  , "find", "first", "five", "followed", "following", "follows", "for"
  , "former", "formerly", "forth", "four", "from", "further", "furthermore"
  , "g", "get", "gets", "getting", "gif", "given", "gives", "go", "goes"
  , "going", "gone", "got", "gotten", "greetings", "h", "had", "hadn't"
  , "happens", "hardly", "has", "hasn't", "have", "haven't", "having"
  , "he", "he'd", "he'll", "he's", "hello", "help", "hence", "her"
  , "here", "here's", "hereafter", "hereby", "herein", "hereupon", "hers"
  , "herself", "hi", "him", "himself", "his", "hither", "hopefully", "how"
  , "how's", "howbeit", "however", "i", "i'd", "i'll", "i'm", "i've"
  , "identify", "ie", "if", "ignored", "immediate", "in", "inasmuch"
  , "inc", "indeed", "indicate", "indicated", "indicates", "inner"
  , "insofar", "instead", "into", "involves", "inward", "is", "isn't"
  , "it", "it'd", "it'll", "it's", "its", "itself", "j", "just", "k"
  , "keep", "keeps", "kept", "know", "known", "knows", "l", "last"
  , "late", "lately", "later", "latter", "latterly", "least", "less"
  , "lest", "let", "let's", "like", "liked", "likely", "little", "look"
  , "looking", "looks", "ltd", "m", "main", "mainly", "many", "may"
  , "maybe", "me", "mean", "meanwhile", "merely", "might", "min", "more"
  , "moreover", "most", "mostly", "much", "must", "mustn't", "my", "myself"
  , "n", "name", "namely", "nd", "near", "nearly", "necessary", "need"
  , "needs", "neither", "never", "nevertheless", "new", "next", "nine"
  , "no", "nobody", "non", "none", "noone", "nor", "normally", "not"
  , "nothing", "novel", "now", "nowhere", "o", "obviously", "of", "off"
  , "often", "oh", "ok", "okay", "old", "on", "once", "one", "ones"
  , "only", "onto", "or", "other", "others", "otherwise", "ought", "our"
  , "ours", "ourselves", "out", "outside", "over", "overall", "own", "p"
  , "particular", "particularly", "per", "perhaps", "placed", "please"
  , "plus", "possible", "presents", "presumably", "probably", "provides"
  , "q", "que", "quite", "qv", "r", "rather", "rd", "re", "really"
  , "reasonably", "regarding", "regardless", "regards", "relatively"
  , "respectively", "right", "s", "said", "same", "saw", "say", "saying"
  , "says", "sds", "second", "secondly", "see", "seeing", "seem", "seemed"
  , "seeming", "seems", "seen", "self", "selves", "sensible", "sent"
  , "serious", "seriously", "seven", "several", "shall", "shan't"
  , "she", "she'd", "she'll", "she's", "should", "shouldn't", "since"
  , "six", "so", "some", "somebody", "somehow", "someone", "something"
  , "sometime", "sometimes", "somewhat", "somewhere", "soon", "sorry"
  , "specified", "specify", "specifying", "still", "sub", "such", "sup"
  , "sure", "t", "t's", "take", "taken", "tell", "tends", "th", "than"
  , "thank", "thanks", "thanx", "that", "that's", "thats", "the", "their"
  , "theirs", "them", "themselves", "then", "thence", "there", "there's"
  , "thereafter", "thereby", "therefore", "therein", "theres", "thereupon"
  , "these", "they", "they'd", "they'll", "they're", "they've", "think"
  , "third", "this", "thorough", "thoroughly", "those", "though", "three"
  , "through", "throughout", "thru", "thus", "to", "together", "too"
  , "took", "toward", "towards", "tried", "tries", "truly", "try"
  , "trying", "twice", "two", "u", "un", "under", "unfortunately"
  , "unless", "unlikely", "until", "unto", "up", "upon", "us", "use"
  , "used", "useful", "uses", "using", "usually", "uucp", "v", "value"
  , "various", "very", "via", "viz", "vs", "w", "want", "wants", "was"
  , "wasn't", "way", "we", "we'd", "we'll", "we're", "we've", "welcome"
  , "well", "went", "were", "weren't", "what", "what's", "whatever", "when"
  , "when's", "whence", "whenever", "where", "where's", "whereafter"
  , "whereas", "whereby", "wherein", "whereupon", "wherever", "whether"
  , "which", "while", "whither", "who", "who's", "whoever", "whole", "whom"
  , "whose", "why", "why's", "will", "willing", "wish", "with", "within"
  , "without", "won't", "wonder", "would", "wouldn't", "x", "y", "yes"
  , "yet", "you", "you'd", "you'll", "you're", "you've", "your", "yours"
  , "yourself", "yourselves", "z", "zero"]


