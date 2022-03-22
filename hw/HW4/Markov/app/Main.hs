module Main where

import           Data.Maybe
import           Markov
import           Utils

main :: IO ()
main = do
  putStrLn "\n** Which corpus? **\n\n1 Brown corpus\n2 Austen\n3 Shakespeare\n4 Great Expectations\n5 Moby Dick\n"
  putStr "> "
  corp <- getLine
  let db = [("1", "brown_notags"), ("2", "austen-ss+e"), ("3", "all_shake"), ("4", "great-expectations"), ("5", "moby-dick")]
  let corpusPath = "text/"++ fromJust (lookup corp db) ++ ".txt"
  x <- readFile corpusPath
  putStrLn $ "\nCorpus: " ++ corpusPath
  putStrLn "\n** Sample how many sentences/lines? **\n  2000  is big enough to get ok results\n  20000 is even better but takes a bit of time"
  putStr "\n> "
  n <- getLine
  let c = map clean $ take (read n :: Int) $ filter (/= "") $ lines x
  let dream0 c = do s <- genSequence (toMatrix $ getUnigrams c) [ ]
                    putStr $ s ++ " "
  let dream1 c = do s <- genSequence (toMatrix $ getBigrams  c) ["<"]
                    putStrLn s
  let dream2 c = do s <- genSequence (toMatrix $ getTrigrams c) ["<","<"]
                    putStrLn s
-- UNCOMMENT OUT THE NEXT 2 LINES TO GET UNIGRAM-BASED OUTPUT (WORD SALAD) --
--  putStrLn "\n** Unigram model (20 words) **"
--  sequence_ $ replicate 20 (dream0 c)
-- ==========================================================================
  putStrLn "\n\n** Bigram model (10 sentences) **"
  sequence_ $ replicate 10 (dream1 c)
  putStrLn "\n** Trigram model (10 sentences) **"
  sequence_ $ replicate 10 (dream2 c)
