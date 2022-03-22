import           Control.Arrow
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S

data Pe = NT String | Eps Char | Nul | Ch Char | Or [Pe] | Pe :. Pe | Del Pe

type Grammar = M.Map String Pe

data Ast = Bad | Lf Char | Ast :@ Ast deriving Show

isBad :: Ast -> Bool
isBad Bad = True
isBad _   = False

parseNull :: Pe -> State Grammar Ast
parseNull pe = leastFix M.empty where
  leastFix guessed = do
    (b, (_, guessed')) <- runStateT (visit pe) (S.empty, guessed)
    if M.size guessed == M.size guessed' then pure b else leastFix guessed'

visit :: Pe -> StateT (S.Set String, M.Map String Ast) (State Grammar) Ast
visit pe = case pe of
  Eps x  -> pure $ Lf x
  Del x  -> visit x
  Nul    -> pure Bad
  Ch _   -> pure Bad
  Or xs  -> chainsaw <$> mapM visit xs
  x :. y -> mul <$> visit x <*> visit y
  NT s -> do
    (seen, guessed) <- get
    case () of
      () | Just x <- M.lookup s guessed -> pure x
         | S.member s seen -> pure Bad
         | otherwise -> do
           modify $ first $ S.insert s
           b <- visit =<< lift (memoDerive s)
           unless (isBad b) $ modify $ second $ M.insert s b
           pure b

mul :: Ast -> Ast -> Ast
mul Bad _ = Bad
mul _ Bad = Bad
mul x y   = x :@ y

-- | Helps cut a non-empty parse forest down to one tree.
chainsaw :: [Ast] -> Ast
chainsaw xs | null xs'   = Bad
            | otherwise  = head xs'
            where xs' = filter (not . isBad) xs

memoDerive :: String -> State Grammar Pe
memoDerive cs@(c:s) = do
  m <- get
  unless (M.member cs m) $ do
    modify $ M.insert cs $ NT cs
    d <- derive c =<< memoDerive s
    modify $ M.insert cs d
  gets (M.! cs)
memoDerive _ = error "unreachable"

derive :: Char -> Pe -> State Grammar Pe
derive c pe = case pe of
  NT s             -> pure $ NT $ c:s
  Ch x | x == c    -> pure $ Eps x
  Or xs            -> Or <$> mapM (derive c) xs
  Del x :. y       -> (Del x :.) <$> derive c y
  x :. y           -> do
    b <- parseNull x
    dx <- derive c x
    if isBad b then pure $ dx :. y else do
      dy <- derive c y
      pure $ Or [dx :. y, Del x :. dy]
  _                -> pure Nul
