module W4 where

import Data.Char
import Data.List
import W3 hiding (main)

getSentences :: String -> [String]
getSentences = lines


clean' xs = filter (\x -> not (isControl x)) (clean xs)
-- scrubbing out some extra garbage
-- equiv to `clean' = filter (not . isControl) . clean`


ngramsRec :: Int -> [a] -> [[a]]
ngramsRec n s =
  let gram = take n s in
    if length gram == n
      then gram : ngramsRec n (tail s)
      else []


ngrams :: Int -> [a] -> [[a]]
ngrams n l = let ts     = tails l
                 shrunk = map (take n) ts in
  filter (\xs -> length xs == n) shrunk
--        aka `(== n) . length`


bigrams :: [a] -> [(a, a)]     -- pairs of two words 
bigrams xs = zip xs (tail xs)  -- rather than lists


trigrams :: [a] -> [(a, a, a)] -- triples
trigrams xs = zip3 xs (tail xs) (tail (tail xs))
-- Data.List offers up to zip7!


tokenize' xs = "<s>" : tokenize xs ++ ["</s>"]


main = do
  raw <- readFile "files/brown_notags.txt"
  let sents = getSentences raw
  let tokened = map (tokenize' . clean') sents
  let grams = concatMap bigrams tokened
  let types = getTypes grams
  let withCount = sort (map addCount types)
  let withFs = relFreqs (length tokened) withCount
  writeFile "out/bigrams.txt" (unlines (map show withFs))


corpus = [ "the girl saw the boy"
         , "the boy kicked the can"
         , "the girl said the boy left"]


