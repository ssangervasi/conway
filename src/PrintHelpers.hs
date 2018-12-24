module PrintHelpers where

puts = putStrLn

quote s = q ++ s ++ q where q = "\""

linesOfWords = unlines . (map unwords)
wordsOfLines = (map words) . lines
