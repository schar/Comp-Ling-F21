{-# LANGUAGE OverloadedStrings #-}

module Regexp (
  Regexp,
  (<|>), (<.>), star, zero, one,
  match, mset, splits,
  anyc, alpha, lower, upper, digit
) where

import           Data.Char
import           GHC.Exts  (IsString (..))
import           Prelude   hiding ((<.>))

-- Adapted from Pedro Vasconcelos, after Danvy & Nielsen "Defunctionalization
-- at work" (2001), Harper "Proof-directed debugging" (1999).

data Regexp = Zero                  -- nothing (0)
            | One                   -- empty string (epsilon)
            | Lit Char              -- single character
            | Cat  Regexp Regexp    -- concatenation (.)
            | Plus Regexp Regexp    -- union (+)
            | Star Regexp           -- repetition (*)
            | Neg  Regexp
            deriving Show

-- Smart constructors. These are what the students are actually exposed to.
infixl 6 <|>
infixl 7 <.>

(<|>) :: Regexp -> Regexp -> Regexp
Zero <|> e = e
e <|> Zero = e
e1 <|> e2  = Plus e1 e2

(<.>) :: Regexp -> Regexp -> Regexp
Zero <.> _ = Zero
_ <.> Zero = Zero
One <.> e  = e
e <.> One  = e
--(Cat e1 e2) <.> e3 = Cat e1 (e2 <.> e3)
e1 <.> e2  = Cat e1 e2

star :: Regexp -> Regexp
star Zero     = One
star One      = One
star (Star e) = Star e
star e        = Star e

zero :: Regexp
zero = Zero

one :: Regexp
one = One

neg (Neg r) = r
neg r       = Neg r
--

type Cont = String -> Bool

accept :: Regexp -> String -> Cont -> Bool  -- worker function
accept Zero    _       _ = False
accept One     cs      k = k cs
accept (Lit c) (c':cs) k = c==c' && k cs
accept (Lit c) _       k = False
accept (Cat  e1 e2) cs k = accept e1 cs (\cs' -> accept e2 cs' k)
accept (Plus e1 e2) cs k = accept e1 cs k || accept e2 cs k
accept (Star e) cs k     = acceptStar e cs k
  where
    acceptStar e cs k
      = k cs || accept e cs (\cs' -> cs'/= cs && acceptStar e cs' k)
-- `acceptStar` piles up as much `accept e cs (\cs' -> ... accept e cs' (...))`
-- as needed to consume all of `cs`. The `cs'/=cs` condition ensures that some
-- of `cs` is consumed on each call -- useful when `e` is `Star One` and `cs`
-- is a non-matching string. The smart constructors actually take care of this
-- already, by normalizing `star One` to `One`.

match :: Regexp -> String -> Bool
match re s = accept re s null

instance IsString Regexp where
  fromString cs = foldr ((<.>) . Lit) One cs

fromChars :: [Char] -> Regexp
fromChars = foldr ((<|>) . fromString . (:[])) Zero

anyc :: Regexp
anyc = fromChars $ map chr [33..126] ++ " \n\r\t"

alpha :: Regexp
alpha = fromChars $ ['A'..'Z'] ++ ['a'..'z']

lower :: Regexp
lower = fromChars $ ['a'..'z']

upper :: Regexp
upper = fromChars $ ['A'..'Z']

digit :: Regexp
digit = fromChars $ ['0'..'9']

mset :: Regexp -> [String]
mset Zero       = []
mset One        = [""]
mset (Lit c)    = [[c]]
mset (Plus r s) = mset r ++ mset s
mset (Cat  r s) = [u++v | u <- mset r, v <- mset s]
mset (Star r)   = concatMap (mset . dup r) [0..]
  where
    dup r n = foldr (<.>) One (replicate n r)

splits :: [a] -> [([a], [a])]
splits u = [splitAt i u | i <- [0..length u]]

match' :: Regexp -> String -> Bool
match' Zero       _  = False
match' One        cs = cs==""
match' (Lit c)    cs = cs==[c]
match' (Plus r s) cs = match' r cs || match' s cs
match' (Cat r s)  cs =
  or [ match' r u && match' s v | (u,v) <- splits cs ]
match' (Star r)   "" = True
match' (Star r)   cs =
  or [ match' r u && match' (star r) v
       | (u,v) <- tail (splits cs) ]
match' (Neg  r)   cs = not (match' r cs)
