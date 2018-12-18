module Conway.Serial where

import Conway.GameOfLife

golToString :: GoL -> String
golToString (GoL cellRows) = 
  let rowStrings = map (concatMap cellToString) cellRows
      nonEmptyRowStrings = filter (not . null) rowStrings
  in unlines nonEmptyRowStrings

stringToGoL :: String -> GoL
stringToGoL str = 
  let cellifyRow = (map stringToCell) . (chunksOf 1)
      cellRows = map cellifyRow $ lines str
  in GoL cellRows

chunksOf :: Int -> [a] -> [[a]]
chunksOf size list =
  let nextSlice :: [a] -> a -> [a]
      nextSlice lastSlice nextItem
        | (length lastSlice) < size = lastSlice ++ [nextItem]
        | otherwise = [nextItem]
      intermediateSlices = scanl nextSlice [] list
      fullSlices = filter ((size ==) . length) intermediateSlices
      remainderSlice = last intermediateSlices
      result
        | (length remainderSlice) == 0 = fullSlices
        | (length remainderSlice) < size = fullSlices ++ [remainderSlice]
        | otherwise = fullSlices
  in result

cellToChar :: Cell -> Char
-- Emoji ⬜️
cellToChar Dead = '\128307'
-- Emoji 🔳
cellToChar Live = '\11036'

charToCell :: Char -> Cell
charToCell chr
  | chr == cellToChar Live = Live
  | chr == cellToChar Dead = Dead

cellToString :: Cell -> String
cellToString cell = [cellToChar cell]

stringToCell :: String -> Cell
stringToCell str
  | length str == 1 = (charToCell . head) str



