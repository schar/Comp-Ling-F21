module Earley where

import           CFParse   hiding (X, main)
import           Data.List

data Sym n t = N n | T t deriving (Eq, Ord)

instance (Show n, Show t) => Show (Sym n t) where
  show (N x) = show x
  show (T x) = show x

type CFRule n t = (n, [Sym n t])
type Grammar n t = [CFRule n t]

type Item n t = (Int, n, [Sym n t], [Sym n t], Int)

convert :: CFG n t -> Grammar n t
convert g = [ (n, [T t]) | n :- t <- g ] ++
            [ (n, [N l, N r]) | n :> (l, r) <- g ]

data X = X deriving (Show, Eq)

anbn = [(X, [T 'a', N X, T 'b']), (X, [T 'a', T 'b']) ]

consequences
  :: (Eq n, Eq t) =>
     Grammar n t -> [t] -> Item n t -> [Item n t] -> [Item n t]
consequences g ws item stored =
  complete item stored ++ predict g item ++ scan ws item
  where
  -- scan :: (Eq n, Eq t) => [t] -> Item n t -> [Item n t]
  scan ws (i, a, alpha, T x:beta, j) =
    [ (i, a, alpha++[T x], beta, j+1) | j < length ws, x==ws!!j ]
  scan _ _ = []
  -- predict :: Eq n => Grammar n t -> Item n t -> [Item n t]
  predict g (_, _, _, N x:_, j) =
    [ (j, b, [], gamma, j) | (b, gamma) <- g, x==b ]
  predict _ _ = []
  -- complete :: Eq n => Item n t -> [Item n t] -> [Item n t]
  complete (i, a, alpha, N x:beta, k) stored =
    [ (i, a, alpha++[N x], beta, j) | (k', b, _, [], j) <- stored,
                                      k==k', b==x ]
  complete (k, b, _, [], j) stored =
    [ (i, a, alpha++[N x], beta, j) | (i, a, alpha, N x:beta, k') <- stored,
                                      k==k', b==x ]
  complete _ _ = []

type Store n t = ([Item n t], [Item n t]) -- (chart, agenda)

exhaustAgenda :: (Eq n, Eq t) => Grammar n t -> [t] -> Store n t -> Store n t
exhaustAgenda _ _ store@(ch, [])             = store
exhaustAgenda g ws (ch, agenda@(item:items)) =
  exhaustAgenda g ws (item:ch, new++items)
    where
    new     = conseqs \\ (ch++agenda)
    conseqs = consequences g ws item ch

display :: (Show n, Show t) => [Item n t] -> IO ()
display [] = return ()
display ((i,n,as,bs,j):xs) = do {display xs; putStrLn x}
  where x = show i++", "++show n++" -> "++show as++" * "++show bs++", "++show j

main = do
  display $ fst x -- (filter (\(_,_,_,ss,_) -> ss == []) (fst x))
  display $ fst y
  where x = exhaustAgenda (sort $ convert eng) (words "Mary saw the elk with John")
            ([], [(0,S,[],[N S],0)])
        y = exhaustAgenda anbn ("aaaaaaabbbbbbb") ([], [(0,X,[],[N X],0)])
