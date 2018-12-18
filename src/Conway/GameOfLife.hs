module Conway.GameOfLife where

import Conway.Grid

-- Game of Life
newtype GoL = GoL (Grid Cell) deriving (Eq, Show, Read)
emptyGoL = GoL []

-- Cell
data Cell = Dead | Live deriving (Eq, Ord, Show, Read)

-- Game transitions
conwayNGenerations :: Int -> GoL -> [GoL]
conwayNGenerations n gol =
  scanl (\gen _ -> nextGeneration gen) gol [1 .. (n - 1)]

nextGeneration :: GoL -> GoL
nextGeneration gol@(GoL cellRows) =
  let nextRow = map (nextCell gol)
  in GoL $ map nextRow $ coordinate cellRows

nextCell :: GoL -> Coords -> Cell
nextCell gol@(GoL cellRows) cellCoords =
  let cell              = gridLookupDefault cellRows Dead cellCoords
      liveNeighborCount = countLiveNeighbors gol cellCoords
  in  nextCellState cell liveNeighborCount

nextCellState :: Cell -> Int -> Cell
nextCellState cell liveNeighborCount
  | (cell == Live) && (liveNeighborCount `elem` [2, 3]) = Live
  | (cell == Dead) && (liveNeighborCount == 3) = Live
  | otherwise = Dead

countLiveNeighbors :: GoL -> Coords -> Int
countLiveNeighbors (GoL grid) (i, j) =
  let getCell   = gridLookupDefault grid Dead
      neighbors = map getCell $ neighborCoords (i, j)
  in  length $ filter (Live ==) neighbors
