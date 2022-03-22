module W9 where


type State = Int
type FST s m = ( [State]       -- I
               , [State]       -- F
               , [Arrow s m] ) -- Δ
type Arrow s m = (State, s, m, State)


fstPenAWriteAs :: FST Char String
fstPenAWriteAs =
  ( [6], [8], -- I, F
    [ (6,'a',"a",6), (6,'b',"",6), (6,'a',"a",7),
      (7,'a',"a",8), (7,'b',"",8) ] )


fstSibHarm :: FST Char String
fstSibHarm =
  ( [1], [1,2,3],
    [ (1,'t',"t",1), (1,'o',"o",1), (1,'s',"s",2),
      (1,'S',"S",3), (2,'t',"t",2), (2,'o',"o",2),
      (2,'s',"s",2), (2,'S',"s",2), (3,'t',"t",3),
      (3,'o',"o",3), (3,'s',"S",3), (3,'S',"S",3) ] )


fstGardenPath :: FST String String
fstGardenPath =
 ( [1], [5,7],
   [ (1,"the","[D",2),      (2,"horse","N",4),
     (4,"raced","VPart",6), (2,"horse","N]",3),
     (6,"inside","P]",3),   (3,"fell","V",5),
     (3,"raced","V",5),     (5,"inside","P",7) ] )


walk delta str p = case str of
  []   -> [p]
  x:xs -> let oneStep = step delta x p
          in  oneStep >>= \p' -> walk delta xs p'


-- walk :: Eq c => [Arrow c String] -> [c] ->
--                 (State, String) -> [(State, String)]


-- transduceRL :: Eq c => FST c String -> [s] -> [String]
transduceRL t w = map reverse (transduce t (reverse w))


composeFST t u s = transduce t s >>= transduce u
--                 [String]          String -> [String]
--                 ------------------------------------
--                          [String]


instance Monoid Double where
  mempty  = 1.0
  mappend = (*)

instance Semigroup Double where -- boilerplate
  (<>) = mappend


data CV = C | V deriving (Eq,Show,Read)

fstCVWeights :: FST CV Double
fstCVWeights = ( [1], [1],
 [ (1,V,0.9,1), (1,C,1.0,2), (2,V,1.0,1),
   (2,V,1.0,3), (1,V,0.9,3), (3,C,0.8,1) ] )


step
  :: (Eq c, Monoid m) =>
     [Arrow c m] -> c -> (State, m) -> [(State, m)]
step delta x (q,m) = [ (r',m<>n) | (r,y,n,r') <- delta,
                                   q==r, y==x ]

transduce
  :: (Eq c, Monoid m) =>
     FST c m -> [c] -> [m]
transduce (i, f, delta) str = outputs
  where
    forward = i >>= \q -> walk delta str (q, mempty)
    outputs = [ t | (qn, t) <- forward, elem qn f ]


