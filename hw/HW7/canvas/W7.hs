module W7 where


type State = Int
type FSA sym = ( [State]            -- Q
               , [sym]              -- Σ
               , [State]            -- I
               , [State]            -- F
               , [Transition sym] ) -- Δ
type Transition sym = (State, sym, State)


data CV = C | V
  deriving (Eq, Show)

fsaCCVV :: FSA CV
fsaCCVV = ( [40,41,42,43], [C,V], [40], [43],
            [(40,C,40),(40,V,40),(40,C,41),(41,C,43),
             (40,V,42),(42,V,43),(43,C,43),(43,V,43)] )


validFSA :: Eq a => FSA a -> Bool
validFSA (states, syms, i, f, ds) =
  let validState q         = elem q states
      validTrans (q,sym,r) = validState q  &&
                             elem sym syms &&
                             validState r  in
  all validState i && -- all odd  [1,3,5] == True
  all validState f && -- all even [2,3,4] == False
  all validTrans ds


{- superseded by W8
focus :: [Transition a] -> State -> [Transition a]
focus delta q = [(r,x,r') | (r,x,r') <- delta, r==q]


step :: Eq a => [Transition a] -> a -> State -> [State]
step delta x q = [ r' | (r,y,r') <- focus delta q, y==x ]


walk :: Eq a => [Transition a] -> [a] -> State -> [State]
walk delta str q =
  case str of
    x:xs -> let nexts = step delta x q
--              nexts :: [State]
                dests q' = walk delta xs q' in
--              dests :: State -> [State]
                concatMap dests nexts
    []   -> [q]
--  good job, you consumed the string, you can rest!


accepts :: Eq a => FSA a -> [a] -> Bool
accepts (states, syms, i, f, delta) str =
  or [ elem qn f | q0 <- i, qn <- walk delta str q0 ]
-}
