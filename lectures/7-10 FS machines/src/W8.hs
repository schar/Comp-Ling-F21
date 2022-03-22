module W8 where

import           Prelude hiding ((>>=))
import           W7

fsaPenultA :: FSA Char
fsaPenultA = ( [6,7,8], ['a','b'], [6], [8],
  [(6,'a',6),(6,'b',6),(6,'a',7),(7,'a',8),(7,'b',8)] )


step :: Eq a => [Transition a] -> a -> State -> [State]
step delta x q = [ r' | (r,y,r') <- delta, q==r, y==x ]


walk :: Eq a => [Transition a] -> [a] -> State -> [State]
walk delta str q =
  case str of
    []   -> [q] -- you consumed the string, rest here!
    x:xs -> let oneStep  = step delta x q -- [S]
                walkTail = walk delta xs  -- S -> [S]
            in  oneStep >>= walkTail


(>>=) :: [State] -> (State -> [State]) -> [State]
xs >>= f = concatMap f xs


parseFSA :: Eq a => FSA a -> [a] -> Bool
parseFSA (_,_,i,f,delta) str = isAccepting walkFromi
  where
    walkFromi      = i >>= walk delta str
    isAccepting qs = or [ elem qn f | qn <- qs ]


