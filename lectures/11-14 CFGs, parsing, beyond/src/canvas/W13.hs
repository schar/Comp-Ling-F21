module W13 where

import           W11
import           W12

type WCFG cat term weight = [(Rule cat term, weight)]
type PCFG cat term = WCFG cat term Double


parseW g [x] = [ (n, w) | (n :- y, w) <- g, y==x ]
parseW g xs  =
  [ (n, w<>wl<>wr) | (ls, rs) <- breaks xs,
                     (nl, wl) <- parseW g ls,
                     (nr, wr) <- parseW g rs,
                     (n :> (l,r), w) <- g, l==nl, r==nr ]


type Stage cat term = ([cat], [term])

shift
  :: Eq term =>
     CFG cat term -> Stage cat term -> [Stage cat term]
shift g (cs, t:ts) = [(n:cs, ts) | n:-x <- g, x==t]

reduce
  :: Eq cat =>
     CFG cat term -> Stage cat term -> [Stage cat term]
reduce g (r:l:cs, ts) = [(n:cs, ts) | n:>(l',r') <- g,
                                      l'==l, r'==r]


step
  :: (Eq cat, Eq term) =>
     CFG cat term -> Stage cat term -> [Stage cat term]
step g st@(r:l:cs, t:ts) = shift g st ++ reduce g st
step g st@(cs    , t:ts) = shift g st
step g st@(r:l:cs, ts)   = reduce g st
step g st                = [st]


parseSR :: (Eq cat, Eq term) =>
             CFG cat term -> [term] -> [Stage cat term]
parseSR g ws = helper g [([], ws)] where
  once stages = stages >>= step g    -- taking 1 step
  helper g stages
    | once stages == stages = stages -- fixed point
    | otherwise             = helper g (once stages)


