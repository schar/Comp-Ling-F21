{-# LANGUAGE OverloadedStrings #-}

module Re (
  Re,
  (<>), (<+>), star, zero, one, neg,
  nullable, derive, mkDfa, minimize,
  anyc, alpha, lower, upper, digit
) where

import           Data.Char
import           Data.List hiding (nub)
import           GHC.Exts  (IsString (..))
import           Prelude   hiding ((<>))

data Re = Zero
        | One
        | Lit  Char
        | Cat  Re Re
        | Alt  [Re]
        | Star Re
        | Neg  Re
  deriving (Eq, Ord)

-- smart constructors
infixl 7 <>
infixl 6 <+>

(<>) :: Re -> Re -> Re
Zero <> _         = Zero
_ <> Zero         = Zero
One <> e          = e
e <> One          = e
(Cat e1 e2) <> e3 = Cat e1 (e2 <> e3)
e1 <> e2          = Cat e1 e2

alt :: [Re] -> Re
alt rs | [r] <- rs' = r
       | [ ] <- rs' = Zero
       | otherwise  = Alt rs'
       where rs' = nub $ rs >>= flatAlt
             flatAlt (Alt as) = as
             flatAlt Zero     = [ ]
             flatAlt a        = [a]
             nub = map head . group . sort

(<+>) :: Re -> Re -> Re
e1 <+> e2 = alt [e1, e2]

star :: Re -> Re
star Zero     = One
star One      = One
star (Star e) = Star e
star e        = Star e

zero :: Re
zero = Zero

one :: Re
one = One

neg (Neg r) = r
neg r       = Neg r

-- derivatives
nullable :: Re -> Bool
nullable re = case re of
  Zero    -> False
  One     -> True
  Lit _   -> False
  Cat r s -> nullable r && nullable s
  Alt rs  -> any nullable rs
  Star _  -> True
  Neg r   -> not (nullable r)

derive :: Char -> Re -> Re
derive c f = case f of
  Zero                 -> Zero
  One                  -> Zero
  Lit a   | a == c     -> One
          | otherwise  -> Zero
  Cat r s | nullable r -> alt [dc r <> s, dc s]
          | otherwise  -> dc r <> s
  Alt rs               -> alt $ dc <$> rs
  Star r               -> dc r <> f
  Neg r                -> neg $ dc r
  where dc = derive c

-- dfa conversion and minimization
type FSA a = ([Re], Re, [Re], [((Re, Re), a)])

mkDfa :: Re -> FSA Char
mkDfa r = (states, r, filter nullable states, edges) where
  (states, edges) = explore ([r], []) r
  explore gr q = foldl' (goto q) gr ['a'..'z']
  goto q (qs, es) c | qc `elem` qs = (qs, es1)
                    | otherwise    = explore (qc:qs, es1) qc
                    where qc  = derive c q
                          es1 = ((q, qc), c):es

minimize :: FSA Char -> FSA Char
minimize (q, r, f, delta) = (q', r, f, delta')
  where
    delta' = sort $ filter (\((r,s),c) -> s /= Zero) delta
    q'     = filter (/= Zero) q

-- conveniences
instance IsString Re where
  fromString cs = foldr ((<>) . Lit) One cs

instance Show Re where
  show Zero      = "0"
  show One       = "1"
  show (Lit c)   = [c]
  show (Cat r s) = "("++ show r ++ show s ++")"
  show (Alt rs)  = "("++ intercalate "|" (map show rs) ++")"
  show (Star r)  = show r ++ "*"
  show (Neg r)   = "~" ++ show r

fromChars :: [Char] -> Re
fromChars = alt . map (fromString . (:[]))

anyc = fromChars $ map chr [33..126] ++ " \n\r\t"

alpha = fromChars $ ['A'..'Z'] ++ ['a'..'z']

lower = fromChars $ ['a'..'z']

upper = fromChars $ ['A'..'Z']

digit = fromChars $ ['0'..'9']
