module Utils where

import           Data.Maybe
import           Data.Tree

-- CFGs in CNF
-- ===========

type CFG cat term = [Rule cat term]

data Rule cat term = cat :- term | cat :> (cat, cat)
  deriving (Eq, Show)

-- LBTs
-- ====

data LBT cat term = Leaf cat term
                  | Branch cat (LBT cat term)
                               (LBT cat term)
  deriving (Eq, Show)

label :: LBT n x -> n
label (Leaf n _)     = n
label (Branch n _ _) = n

-- A small grammar for English
-- ===========================

data Cat = S | D | DP | NP | VT | VP | P | PP | WP | W | RC | C
  deriving (Eq, Read, Show)

eng :: [Rule Cat String]
eng = [ S  :> (DP, VP)    ,
        S  :> (WP, S )    , -- preposed while-phrases
        WP :> (W , S)     , -- making a while-phrase
        VP :> (VT, DP)    ,
        DP :> (D , NP)    ,
        NP :> (NP, PP)    ,
        NP :> (NP, RC)    , -- relative clause modification of an NP
        PP :> (P , DP)    ,
        VP :> (VP, PP)    ,
        RC :> (C, VP)     , -- subject-gap rel clauses require "that"
        RC :> (DP, VT)    , -- object-gap rel clauses without "that"
        DP :- "Sam"       ,
        DP :- "I"         ,
        VT :- "wrote"     ,
        VT :- "know"      ,
        VT :- "watched"   , -- "watched" is transitive
        VP :- "watched"   , -- "watched" is also intransitive
        VP :- "cried"     ,
        VP :- "awoke"     ,
        VP :- "stinks"    ,
        D  :- "the"       ,
        D  :- "this"      ,
        NP :- "book"      ,
        NP :- "baby"      ,
        P  :- "with"      ,
        C  :- "that"      ,
        W  :- "while"     ]

-- Naive parsing (to cats and LBTs)
-- =================================

breaks :: [a] -> [([a], [a])]
breaks u = [splitAt i u | i <- [1..length u - 1]]

parse -- parsing to cat's
  :: (Eq cat, Eq term) =>
     CFG cat term -> [term] -> [cat]
parse g [x] = [ n | n :- y <- g, y==x ]
parse g xs  = [ n | (ls,rs) <- breaks xs,
                    nl <- parse g ls, nr <- parse g rs,
                    n :> (l,r) <- g, l==nl, r==nr ]

parseToLBT :: (Eq cat, Eq term) => -- parsing to LBTs
     CFG cat term -> [term] -> [LBT cat term]
parseToLBT g [x] = [ Leaf n x | n :- y <- g, y==x ]
parseToLBT g xs  =
  [ Branch n tl tr | (ls, rs) <- breaks xs,
        tl <- parseToLBT g ls, tr <- parseToLBT g rs,
        n :> (l, r) <- g, label tl == l, label tr == r ]

-- CYK parsing (just to cats)
-- ==========================

mkChart :: (Eq cat, Eq term) =>
  CFG cat term -> [term] -> [((Int, Int), [cat])]
mkChart g xs = helper (0,1) [] where
  helper p@(i,j) tab
    | j>length xs = tab
    | i<0         = helper (j,j+1) tab
    | i==j-1      = helper (i-1,j) $
                      (p, [n | n:-t <- g, t==xs!!(j-1)]):tab
    | otherwise   = helper (i-1,j) $
                      (p, [n | n:>(l,r) <- g, k <- [i+1..j-1],
                               lc <- fromJust $ lookup (i,k) tab, l==lc,
                               rc <- fromJust $ lookup (k,j) tab, r==rc]):tab

parseCYK :: (Eq cat, Eq term) => CFG cat term -> [term] -> [cat]
parseCYK g xs = snd $ head $ mkChart g xs

-- Pretty-printed outputs
-- ======================

toTree :: (Show cat, Show term) =>
          LBT cat term -> Tree String -- convert LBT to Tree
toTree (Leaf   n x)   = Node (show n) [Node (show x) []]
toTree (Branch n l r) = Node (show n) [toTree l, toTree r]

displayForest :: (Show cat, Show term) =>
                 [LBT cat term] -> IO ()
displayForest = putStrLn . drawForest . map toTree


-- Shift-reduce parsing
-- ====================

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

stepSR
  :: (Eq cat, Eq term) =>
     CFG cat term -> Stage cat term -> [Stage cat term]
stepSR g st@(r:l:cs, t:ts) = shift g st ++ reduce g st
stepSR g st@(cs    , t:ts) = shift g st
stepSR g st@(r:l:cs, ts)   = reduce g st
stepSR g st                = [st]

-- Top-down (predict-match) parsing
-- ================================

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
