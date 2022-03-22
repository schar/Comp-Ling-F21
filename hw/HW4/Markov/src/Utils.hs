module Utils where

import           Data.Char
import           Data.List

repDash :: String -> String
repDash xs = map (\c -> if c == '-' then ' ' else c) xs

delPunct :: String -> String
delPunct xs = filter (\x -> isAlphaNum x || isSpace x) xs

allLower :: String -> String
allLower xs = map toLower xs

clean :: String -> String
clean = allLower . delPunct . repDash

getTypes :: [String] -> [[String]]
getTypes = group . sort

addCount :: [String] -> (Int, String)
addCount xs = (length xs, head xs)

ezDiv :: Int -> Int -> Double
ezDiv m n = fromIntegral m / fromIntegral n

relFreqs :: Int -> [(Int, String)] -> [(Double, String)]
relFreqs n xs = let f = (\(m, w) -> (ezDiv m n, w)) in
  map f xs

linesXML s =
  let ls     = lines s
      f      = \l -> take 6 l == "<LINE>"
      lsFilt = filter f ls  in
  map (takeWhile (/= '<') . drop 6) lsFilt
