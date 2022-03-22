import           Data.List hiding (nub)

data Re = Nul | Eps | Lit Char | Kleene Re | Re :. Re | Alt [Re]
  deriving (Eq, Ord, Show)

nullable :: Re -> Bool
nullable re = case re of
  Nul      -> False
  Eps      -> True
  Lit _    -> False
  r :. s   -> nullable r && nullable s
  Alt rs   -> any nullable rs
  Kleene _ -> True

derive :: Char -> Re -> Re
derive c f = case f of
  Nul                 -> Nul
  Eps                 -> Nul
  Lit a  | a == c     -> Eps
         | otherwise  -> Nul
  r :. s | nullable r -> mkAlt [dc r :. s, dc s]
         | otherwise  -> dc r :. s
  Alt rs              -> mkAlt $ dc <$> rs
  Kleene r            -> dc r :. f
  where dc = derive c

mkDfa :: Re -> ([Re], Re, [Re], [((Re, Re), Char)])
mkDfa r = (states, r, filter nullable states, edges) where
  (states, edges) = explore ([r], []) r
  explore gr q = foldl' (goto q) gr ['a'..'c']
  goto q (qs, es) c | qc `elem` qs = (qs, es1)
                    | otherwise    = explore (qc:qs, es1) qc
                    where qc  = derive c q
                          es1 = ((q, qc), c):es

mkAlt :: [Re] -> Re
mkAlt rs | [r] <- rs' = r
         | otherwise  = Alt rs'
         where rs' = nub $ concatMap flatAlt rs
               flatAlt (Alt as) = as
               flatAlt a        = [a]
               nub = map head . group . sort
