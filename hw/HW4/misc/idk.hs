import           Data.Char
import           Data.List
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           System.Random
import           W3            hiding (main)

main = do
  s <- readFile "brown_nolines.txt"
  putStrLn "n grams?"
  n <- getLine
  let rn = read n
  let sentences = filter (/= "\r") $ lines s
  let tokens = tokenize . clean
  let wrap = (replicate (rn-1) "<s>" ++) . (++ replicate (rn-1) "</s>")
  let ready = map (wrap . tokens) sentences
  putStr "thinking..."
  let grams = concatMap (trigrams') ready
  --putStrLn $ show $ length bigrams
  writeFile "out.txt" $ unlines $ map show (nubOrdCount grams)

type Ngram = [String]

ngrams :: Int -> [a] -> [[a]]
ngrams n l = let ts     = tails l
                 shrunk = map (take n) ts in
  take ((length l - n) + 1) shrunk

rmTags ""       = ""
rmTags ('_':xs) = rmTags (dropWhile isUpper xs)
rmTags (x:xs)   = x : rmTags xs

ngrams' n l = filter ((== n) . length) aux where
  aux = map (take n) (tails l)

bigrams  xs = zip xs (tail xs)
trigrams xs = zip3 xs (tail xs) (tail (tail xs))
trigrams' xs = let t = tail xs in zip3 xs t (tail t)


nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

nubOrdCount :: Ord a => [a] -> [(Int, a)]
nubOrdCount = sort . map (\xs -> (length xs, head xs)) . group . sort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a < x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

sortx :: (Ord a) => [a] -> [a]
sortx = mergeAll . map (:[])
  where
    mergeAll []  = []
    mergeAll [t] = t
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (x:y:xs) = merge x y:mergePairs xs
    mergePairs xs       = xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
