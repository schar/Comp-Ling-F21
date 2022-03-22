module W3 where

import           Data.Char
import           Data.List
import qualified Data.Set  as Set

whatsThisDo1 :: [a] -> [a]
whatsThisDo1 []     = []
whatsThisDo1 (x:xs) = x : whatsThisDo1 xs


whatsThisDo2 :: [a] -> [a]
whatsThisDo2 []     = []
whatsThisDo2 (x:xs) = whatsThisDo2 xs ++ [x]


isPalindrome :: String -> Bool
isPalindrome xs = xs == whatsThisDo2 xs


-- using recursion
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 f []     = []
myFilter1 f (x:xs) = if f x
                      then x : (myFilter1 f xs)
                      else myFilter1 f xs


-- using a list comprehension
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f [] = []
myFilter2 f xs = [ x | x <- xs, f x ]


-- using recursion
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f []     = []
myMap1 f (x:xs) = f x : (myMap1 f xs)


-- using a list comprehension
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f [] = []
myMap2 f xs = [ f x | x <- xs ]


-- replace dashes with spaces
repDash :: String -> String
repDash xs = map (\c -> if c == '-' then ' ' else c) xs


-- delete punctuation
delPunct :: String -> String
delPunct xs = filter (\x -> isLetter x || isSpace x) xs


-- make all lowercase
allLower :: String -> String
allLower xs = map toLower xs


-- cleaning is doing one after the other
clean :: String -> String
clean xs = allLower (delPunct (repDash xs))
-- first replace dashes with spaces
-- then delete punctuation
-- finally, make all characters lowercase


cleanComp :: String -> String
cleanComp = allLower . delPunct . repDash
--              3rd       (2nd        1st    )


tokenize :: String -> [String]
tokenize = words


allLengths :: [String] -> [Int]
allLengths xs = map length xs


-- firstTry xs = sum (allLengths xs) / length xs
-- won't work:
-- <interactive>:14:15: error:
--     • Could not deduce (Fractional Int) arising from
--       a use of ‘/’
--       from the context: Foldable t


ezDiv :: Int -> Int -> Double -- a floating point number
ezDiv m n = fromIntegral m / fromIntegral n

avgWordLength :: [String] -> Double
avgWordLength xs =
  let numerator   = sum (allLengths xs)
      denominator = length xs in
    ezDiv numerator denominator


getTypes :: Ord a => [a] -> [[a]]
getTypes = group . sort


addCount :: [a] -> (Int, a)
addCount xs = (length xs, head xs)


relFreqs :: Int -> [(Int, a)] -> [(Double, a)]
relFreqs n xs = let f = (\(m, w) -> (ezDiv m n, w)) in
  map f xs -- n is the total word count


main :: IO ()
main = do
  raw <- readFile "aux/great-expectations.txt"
  let cleaned = clean raw
  let tokens = tokenize cleaned
  let types = getTypes tokens
  let withCounts = sort (map addCount types)
  let finally = relFreqs (length tokens) withCounts
  writeFile "aux/freqs.txt" (unlines (map show finally))
  -- don't worry about how this last line works


