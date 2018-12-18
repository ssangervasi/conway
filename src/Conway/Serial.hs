module Conway.Serial where

import qualified Data.Text as T

import Conway.GameOfLife

golToStr :: GoL -> String
golToStr (GoL cellRows) = 
  let rowStrings = map (concatMap cellToStr) cellRows
      nonEmptyRowStrings = filter (not . null) rowStrings
  in unlines nonEmptyRowStrings

strToGoL :: String -> GoL
strToGoL str = 
  let cellifyRow = (map (strToCell . T.unpack)) . (T.chunksOf 1)
      cellRows = map cellifyRow $ T.lines (T.pack str)
  in GoL cellRows

slices :: Int -> [a] -> [[a]]
slices size list =
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

cellToStr :: Cell -> String
cellToStr Live = "🔳"
cellToStr _    = "⬜️"

strToCell :: String -> Cell
strToCell str
  | str == "🔳" = Live
  | otherwise   = Dead
