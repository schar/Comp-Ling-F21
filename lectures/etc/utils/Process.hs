import           Data.List

main = do
  putStrLn ">> input week number"
  n <- getLine
  let week = "W" ++ n
  let str = "455-" ++ week ++ "-2021.md"
  putStrLn $ ">> reading " ++ str
  s <- readFile str
  let sLines = lines s
  putStrLn $ ">> writing " ++ week ++ ".hs"
  writeFile (week ++ ".hs") $
    writePragmas (getYAML "pragmas" sLines) ++ "\n" ++
    "module " ++ week ++ " where" ++ "\n\n" ++
    writeImports (getYAML "imports" sLines) ++ "\n" ++
    unlines (off sLines)
  putStrLn ">> done"

type Line = String

off :: [Line] -> [Line]
off []                   = []
off ("```{.haskell}":xs) = on xs
off (x:xs)               = off xs

on :: [Line] -> [Line]
on []         = []
on ("```":xs) = "\n" : off xs
on (x:xs)     = x : on xs

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                   "" -> []
                   s' -> w : splitOn p s''
                         where (w, s'') = break p s'

getYAML :: String -> [Line] -> [String]
getYAML y ls = map (dropWhile (==' ')) $ splitOn (==',') (drop 9 (line dropping))
  where dropping = dropWhile (not . isPrefixOf (y ++ ": ")) ls
        line [] = ""
        line ls = head ls

writeImports :: [String] -> String
writeImports = foldr (\x acc -> "import " ++ x ++ "\n" ++ acc) ""

writePragmas :: [String] -> String
writePragmas = foldr (\x acc -> "{-# LANGUAGE " ++ x ++ " #-}\n" ++ acc) ""
