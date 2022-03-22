{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Data.Char
import           Data.List
--import qualified Data.Text              as T
--import qualified Graphics.Vega.VegaLite as V

main = do
  s <- readFile "aux/brown.txt"
  let cooked = cook s
  let allN = lenFrac cooked
  let grouped = group (sort cooked)
  let difN = lenFrac grouped
  let withNs = sortBy (flip compare) $ map (\x -> (lenFrac x, x)) grouped
  let csv = makeCsv withNs allN
  writeFile "brown-out.csv" csv
  let columns = unzip3 (rFreqStr 1 allN withNs)
  --B.writeFile "vega" (A.encodePretty x)
  --let x = V.fromVL $ V.toVegaLite [makeData columns]
  --writeFile "vega" $ show x
  putStrLn $ "# tokens " ++ show (toInteger allN)
  putStrLn $ "# types  " ++ show (toInteger difN)

cook :: String -> [String] -- makes 4 passes :/
cook = words . cleanText
  where cleanText = allLower . delPunct . repDash
        repDash  = map (\c -> if c == '-' then ' ' else c)
        delPunct = filter (\x -> isLetter x || isSpace x)
        allLower = map toLower

lenFrac :: Num b => [a] -> b
lenFrac = fromIntegral . length

rFreqStr cur toks []          = []
rFreqStr cur toks ((n,xs):ys) = new : rFreqStr (cur+1) toks ys
  where new = (head xs, cur, n/toks)

makeCsv withFreqs len = "word,rank,freq\n" ++
  unlines (map (init.tail.show) (rFreqStr 1 len withFreqs)) ++ "\n"

--makeData (word,rank,freq) = V.dataFromColumns []
--  . V.dataColumn "word" (V.Strings (map T.pack word))
--  . V.dataColumn "rank" (V.Numbers rank)
--  . V.dataColumn "freq" (V.Numbers freq) $ []
{-
{
  "data": {
    "values": [
      {"a": "C", "b": 2}, {"a": "C", "b": 7}, {"a": "C", "b": 4},
      {"a": "D", "b": 1}, {"a": "D", "b": 2}, {"a": "D", "b": 6},
      {"a": "E", "b": 8}, {"a": "E", "b": 4}, {"a": "E", "b": 7}
    ]
  },
  "mark": "point",
  "encoding": {
    "x": {"field": "a", "type": "nominal"},
    "y": {"field": "b", "type": "quantitative"}
  }
}
-}
