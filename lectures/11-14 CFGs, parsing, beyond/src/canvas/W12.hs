module W12 where

import           Data.Maybe
import           Data.Tree
import           W11

breaks :: [a] -> [([a], [a])]
breaks u = [splitAt i u | i <- [1..length u - 1]]


parse
  :: (Eq cat, Eq term) =>
     CFG cat term -> [term] -> [cat]
parse g [x] = [ n | n :- y <- g, y==x ]
parse g xs  = [ n | (ls,rs) <- breaks xs,
                    -- break the string
                    nl <- parse g ls, nr <- parse g rs,
                    -- parse the two halves
                    n :> (l,r) <- g, l==nl, r==nr ]
                    -- find a corresponding rule


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


parseToLBT :: (Eq cat, Eq term) =>
     CFG cat term -> [term] -> [LBT cat term]
parseToLBT g [x] = [ Leaf n x | n :- y <- g, y==x ]
parseToLBT g xs  =
  [ Branch n tl tr | (ls, rs) <- breaks xs,
        tl <- parseToLBT g ls, tr <- parseToLBT g rs,
        n :> (l, r) <- g, label tl == l, label tr == r ]


toTree :: (Show cat, Show term) =>
          LBT cat term -> Tree String -- convert LBT to Tree
toTree (Leaf   n x)   = Node (show n) [Node (show x) []]
toTree (Branch n l r) = Node (show n) [toTree l, toTree r]

displayForest :: (Show cat, Show term) =>
                 [LBT cat term] -> IO ()
displayForest = putStrLn . drawForest . map toTree
