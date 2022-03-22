module W11 where


type CFG cat term = [Rule cat term]

data Rule cat term = cat :- term | cat :> (cat, cat)
  deriving (Eq, Show) -- infix data constructors! cf.
                      -- A -> x
                      -- A -> BC


data Cat = S | D | DP | NP | VT | VP | P | PP
  deriving (Eq, Read, Show)

eng :: [Rule Cat String]
eng = [ S  :> (DP, VP)    ,
        VP :> (VT, DP)    ,
        DP :> (D , NP)    ,
        NP :> (NP, PP)    ,
        PP :> (P , DP)    ,
        VP :> (VP, PP)    ,
        DP :- "Mary"      ,
        VT :- "saw"       ,
        D  :- "the"       ,
        NP :- "binoculars",
        NP :- "elk"       ,
        P  :- "with"      ]


data LBT cat term = Leaf cat term
                  | Branch cat (LBT cat term)
                               (LBT cat term)
  deriving (Eq, Show) -- Labeled Binary Trees


t0 :: LBT Cat String
t0 = Branch NP
       (Leaf NP "elk")
       (Branch PP
         (Leaf P "with")
         (Branch DP
           (Leaf D "the")
           (Leaf NP "binoculars")))


generates :: (Eq n, Eq x) => CFG n x -> LBT n x -> Bool
generates g t = case t of
  Leaf   n x   -> elem (n :- x) g
  Branch n l r -> elem (n :> (label l, label r)) g &&
                  generates g l && generates g r

label :: LBT n x -> n
label (Leaf n _)     = n
label (Branch n _ _) = n
