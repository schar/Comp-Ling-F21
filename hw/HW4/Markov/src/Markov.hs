module Markov where

import qualified Control.Monad.Random as R
import qualified Data.Map             as M
import           Utils

type Hist = [String] -- prior context as list of words
type Next = String
type Ngrams = [(Hist, Next)]

type NextF = (Next, Rational)
type Matrix = M.Map Hist [NextF]

genSequence :: (R.MonadRandom m) => Matrix -> Hist -> m String
genSequence tm [] = R.fromList $ tm M.! []
genSequence tm src
  | all (== ">") src = return $ unwords src
  | otherwise = next src
  where
    next src = do
      sn <- R.fromList $ tm M.! src           -- get next word
      ss <- genSequence tm (tail src ++ [sn]) -- go w/new hist
      return $ unwords [head src, ss]

addNextF :: NextF -> [NextF] -> [NextF] -- isnt this a Map?
addNextF (t, f) ts = case lookup t ts of
  Nothing -> (t, f) : ts
  Just n  -> (t, n+f) : filter notT ts
    where
      notT (r, _) = r /= t

addNextFs :: [NextF] -> [NextF] -> [NextF]
addNextFs tsA tsB = foldr addNextF tsB tsA

toMatrix :: Ngrams -> M.Map Hist [NextF]
toMatrix = foldr insert M.empty
  where
    insert t = M.insertWith addNextFs (fst t) [(snd t, 1.0)]

s = ("<":)
e = (++" >")

getUnigrams :: [String] -> Ngrams
getUnigrams ls = map (\a -> ([],a)) $ ls >>= words

getBigrams :: [String] -> Ngrams
getBigrams ls = map (\(a,b) -> ([a],b)) zipd
  where
    zipd = map e ls >>= (\ws -> zip (s ws) ws) . words

getTrigrams :: [String] -> Ngrams
getTrigrams ls = map (\(a,b,c) -> ([a,b],c)) zipd
  where
    zipd = map (e.e) ls >>= (\ws -> zip3 (s.s $ ws) (s ws) ws) . words
