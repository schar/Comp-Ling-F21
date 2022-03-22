{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FSTs where

import           Prelude hiding (Monoid (..), Semigroup (..))

type State = Int
type FST s m = ( [State]       -- I
               , [State]       -- F
               , [Arrow s m] ) -- Δ
type Arrow s m = (State, s, m, State) -- Caveat

step :: (Eq s, Monoid m) =>
        [Arrow s m] -> s -> (State, m) -> [(State, m)]
step delta x (q, m) = [ (r', m <> n) | (r, y, n, r') <- delta,
                                       q==r, y==x ]
step' :: Eq c => [Arrow c String] -> c ->
                (State, String) -> [(State, String)]
step' delta x (q,m) = [ (r',m<>n) | (r,y,n,r') <- delta,
                                   q==r, y==x ]

walk :: (Eq s, Monoid m) =>
        [Arrow s m] -> [s] -> (State, m) -> [(State, m)]
walk delta str p = case str of
  []   -> [p]
  x:xs -> let oneStep  = step delta x p
              walkTail = walk delta xs
          in  oneStep >>= walkTail

transduce :: (Eq s, Monoid m) => FST s m -> [s] -> [m]
transduce (i, f, delta) str = outputs
  where
    walkFromi = i >>= \q -> walk delta str (q, one)
    outputs   = [ t | (qn, t) <- walkFromi, elem qn f ]

summarize :: Semiring a => [a] -> a
summarize ts = foldr (<+>) zero ts

-- Classes
class Monoid m where
  (<>) :: m -> m -> m
  one  :: m
class Monoid m => Semiring m where
  (<+>) :: m -> m -> m
  zero  :: m

-- Instances
instance Monoid String where
  (<>) = (++)
  one  = ""

type Weight = Float
instance Monoid Weight where
  (<>) = (*)
  one  = 1
instance Semiring Weight where
  (<+>) = max
  zero  = 0

type Prob = Double
instance Monoid Prob where
  (<>) = (*)
  one  = 1
instance Semiring Prob where
  (<+>) = (+)
  zero  = 0

{-
type Cost = Float
instance Monoid Cost where
  (<>) = (+)
  one  = 0
instance Semiring Cost where
  (<+>) = min
  zero  = 1/0
-}

instance Monoid Bool where
  (<>) = (&&)
  one  = True
instance Semiring Bool where
  (<+>) = (||)
  zero  = False

