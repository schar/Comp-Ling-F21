{-# LANGUAGE DeriveFunctor #-}

module CFTrans where

import           CFParse

type Stage cat term = ([cat], [term])

protoParse step start g ws = helper g $ return (start, ws)
  where
    helper g sts
      | oneStep == sts = sts
      | otherwise = helper g oneStep
          where oneStep = sts >>= step g

-- bottom up
--
shift :: Eq term =>
         CFG cat term -> Stage cat term -> [Stage cat term]
shift g (cs, t:ts) = [(n:cs, ts) | n:-x <- g, x==t]

reduce :: Eq cat =>
          CFG cat term -> Stage cat term -> [Stage cat term]
reduce g (r:l:cs, ts) = [(n:cs, ts) | n:>(l',r') <- g, l'==l, r'==r]

stepSR :: (Eq cat, Eq term) =>
          CFG cat term -> Stage cat term -> [Stage cat term]
stepSR g st@(r:l:cs, t:ts) = shift g st ++ reduce g st
stepSR g st@(cs    , t:ts) = shift g st
stepSR g st@(r:l:cs, ts)   = reduce g st
stepSR g st                = return st

stepSRTree g st@(r:l:cs, t:ts) = shiftTree g st ++ reduceTree g st
stepSRTree g st@(cs    , t:ts) = shiftTree g st
stepSRTree g st@(r:l:cs, ts)   = reduceTree g st
stepSRTree g st                = return st

shiftTree g (cs, t:ts) = [(Leaf n x:cs, ts) | n:-x <- g, x==t]
reduceTree g (r:l:cs, ts) = [(Branch n l r:cs, ts) | n:>(l',r') <- g,
                              l'==label l, r'==label r]

parseSR :: (Eq cat, Eq term) =>
           CFG cat term -> [term] -> [Stage cat term]
parseSR = protoParse stepSR []

parseSRTree :: (Eq cat, Eq term) =>
           CFG cat term -> [term] -> [Stage (LBT cat term) term]
parseSRTree = protoParse stepSRTree []

parseAg g x@(stages,[])  = x
parseAg g (stages, x:xs) = parseAg g (x:stages, xs++stepSR g x)

-- top down
--
predict :: Eq cat =>
           CFG cat term -> Stage cat term -> [Stage cat term]
predict g (a:cs, ts) = [(l:r:cs, ts) | n:>(l,r) <- g, n==a]

match :: (Eq cat, Eq term) =>
         CFG cat term -> Stage cat term -> [Stage cat term]
match g (a:cs, t:ts) = [(cs, ts) | n:-x <- g, n==a, x==t]

stepTD :: (Eq cat, Eq term) =>
          CFG cat term -> ([cat], [term]) -> [Stage cat term]
stepTD g st@(a:cs, t:ts) = match g st ++ predict g st
stepTD g st@(a:cs, ts)   = match g st
stepTD g st              = return st

parseTD :: (Eq cat, Eq term) =>
           cat -> CFG cat term -> [term] -> [Stage cat term]
parseTD c = protoParse stepTD [c]
