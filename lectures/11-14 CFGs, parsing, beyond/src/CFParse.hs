{-# LANGUAGE NoMonomorphismRestriction #-}

module CFParse where

import           Data.Maybe
import           Data.Tree

breaks :: [a] -> [([a], [a])]
breaks u = [splitAt i u | i <- [1..length u - 1]]

data Rule n x = n :- x | n :> (n, n)
  deriving (Eq, Show)

type CFG n x = [Rule n x]

parse
  :: (Eq cat, Eq term) =>
     [Rule cat term]-> [term] -> [cat]
parse g [x] = [ n | n :- y <- g, y==x ]
parse g xs  = [ n | (ls,rs) <- breaks xs,
                    nl <- parse g ls, nr <- parse g rs,
                    n :> (l,r) <- g, l==nl, r==nr ]

data Cat = S | D | DP | NP | VT | VD | VP | P | PP
  deriving (Eq, Ord, Show)

eng :: [Rule Cat String]
eng = [ S  :> (DP, VP)    ,
        VP :> (VT, DP)    ,
        VT :> (VD, DP)    ,
        DP :> (D , NP)    ,
        NP :> (NP, PP)    ,
        PP :> (P , DP)    ,
        VP :> (VP, PP)    ,
        DP :- "Mary"      ,
        DP :- "John"      ,
        VT :- "saw"       ,
        VD :- "gave"      ,
        VP :- "left"      ,
        D  :- "the"       ,
        NP :- "binoculars",
        NP :- "elk"       ,
        P  :- "with"      ]

parseToLBT
  :: (Eq x, Eq n) =>
     CFG n x -> [x] -> [LBT n x]
parseToLBT g [x] = [ Leaf n x | n :- y <- g, y==x ]
parseToLBT g xs  =
  [ Branch n tl tr |
      (ls, rs) <- breaks xs,
      tl <- parseToLBT g ls, tr <- parseToLBT g rs,
      n :> (l, r) <- g, label tl == l, label tr == r ]

toTree :: (Show x, Show n) => LBT n x -> Tree String
toTree (Leaf   n x)   = Node (show n) [Node (show x) []]
toTree (Branch n l r) = Node (show n) [toTree l, toTree r]

s1 :: [String]
s1 = words "Mary saw the elk with the binoculars"

s2 :: [String]
s2 = words "Mary gave the elk with the binoculars the binoculars"

s3 :: [String]
s3 = map return "aaabbb"

displayForest :: (Show x, Show n) => [LBT n x] -> IO ()
displayForest = putStrLn . drawForest . map toTree

main :: IO ()
main = do
  displayForest (parseToLBT eng  s1)
  displayForest (parseToLBT eng  s2)
  displayForest (parseToLBT lbal s3)
  displayForest (parseToLBT rbal s3)

data ANBN = X | A | B | R deriving (Eq, Read, Show)

lbal = [ X :> (A, B),
         X :> (R, B),
         R :> (A, X),
         A :- "a"   ,
         B :- "b"   ]

rbal = [ X :> (A, B),
         X :> (A, R),
         R :> (X, B),
         A :- "a"   ,
         B :- "b"   ]

data LBT cat term = Leaf cat term
                  | Branch cat (LBT cat term)
                               (LBT cat term)
  deriving (Eq, Show) -- Labeled Binary Trees

generates :: (Eq n, Eq x) => CFG n x -> LBT n x -> Bool
generates g t = case t of
  Leaf   n x   -> elem (n :- x) g
  Branch n l r -> elem (n :> (label l, label r)) g &&
                       generates g l && generates g r

label :: LBT n x -> n
label (Leaf n _)     = n
label (Branch n _ _) = n

t0 :: LBT Cat String
t0 =
  Branch NP
    (Leaf NP "elk")
    (Branch PP
      (Leaf P "with")
      (Branch DP
        (Leaf D "the")
        (Leaf NP "binoculars")
      )
    )

cyk g t = [2 | i <- [1..length t]]

parseCKY' g w = gs
  where
    n = length w
    tabulate f = table
      where
        table = [[f i j | j<-[1..n-i+1]] | i<-[1..n]]
        --f'    = \i j -> table !! i !! j
    gs = tabulate gs'
      where
        gs' 1 j = [n | n:-t <- g, w!!(j-1)==t ]
        gs' i j = [n | n:>(l,r) <- g,
                       not $ null
                         [k | k<-[1..i-1],
                              l `elem` gs!!(k-1)!!(j-1),
                              r `elem` gs!!(i-k-1)!!(j+k-1)]]

pas g w = table
  where
    n = length w
    table = [[gs i j|j<-[1..n-i+1]]|i<-[1..n]]
    gs 1 j = [n | n:-t <- g, w !! (j-1)==t ]
    gs i j = [n | k<-[1..i-1], n:>(l,r) <- g,
              l `elem` table!!(k-1)!!(j-1),
              r `elem` table!!(i-k-1)!!(j+k-1)]

mkChart
  :: (Eq cat, Eq term) =>
     CFG cat term -> [term] -> [((Int, Int), [cat])]
mkChart g xs = helper (0,1) []
  where
  helper p@(i,j) table
    | j>length xs = table
    | i==(-1)     = helper (j,j+1) table
    | i==j-1      = helper (i-1,j) $
                      (p, [n | n:-t <- g, t==xs!!(j-1)]) : table
    | otherwise   = helper (i-1,j) $
                      (p, [n | n:>(l,r) <- g, k <- [i+1..j-1],
                               lc <- fromJust $ lookup (i,k) table, l==lc,
                               rc <- fromJust $ lookup (k,j) table, r==rc]) : table

mkChartTree
  :: (Eq cat, Eq term) =>
     CFG cat term -> [term] -> [((Int, Int), [LBT cat term])]
mkChartTree g xs = helperTree (0,1) []
  where
  helperTree p@(i,j) table
    | j>length xs = table
    | i==(-1)     = helperTree (j,j+1) table
    | i==j-1      = helperTree (i-1,j) $
                      (p, [Leaf n t | n:-t <- g, t==xs!!(j-1)]) : table
    | otherwise   = helperTree (i-1,j) $
                      (p, [Branch n tl tr | n:>(l,r) <- g, k <- [i+1..j-1],
                            tl <- fromJust $ lookup (i,k) table, l==label tl,
                            tr <- fromJust $ lookup (k,j) table, r==label tr]) : table

parseCYK
  :: (Eq cat, Eq term) =>
  CFG cat term -> [term] -> [cat]
parseCYK g xs = snd (head (mkChart g xs))
