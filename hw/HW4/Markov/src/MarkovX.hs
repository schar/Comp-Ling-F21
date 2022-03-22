module MarkovX where

import           Control.Arrow
import qualified Data.Map      as Map
import           Data.Maybe
import           System.Random

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs = first (xs!!) . randomR (0, length xs - 1)

main' :: IO ()
main' = do
  text <- readFile "great-expectations.txt"
  g    <- newStdGen
  putStrLn . unwords . take 100 . map fst . (`iterate` ("The", g)) . uncurry $
    \prev -> choose . fromJust . Map.lookup prev . Map.fromListWith (++) .
             (map.second) pure . (zip <*> tail) $ words text
