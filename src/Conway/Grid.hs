module Conway.Grid where

type Grid a = [[a]]
type Coords = (Int, Int)

coordinate :: Grid a -> Grid Coords
coordinate [] = []
coordinate cellRows =
  let rowCount = length cellRows
      coordinateCols row rowIndex =
        let colCount = length row
        in  [ (rowIndex, colIndex) | colIndex <- [0 .. (colCount - 1)] ]
  in zipWith coordinateCols cellRows [0 .. (rowCount - 1)]

gridLookupDefault :: Grid a -> a -> Coords -> a
gridLookupDefault grid d (i, j)
  | i < 0 || j < 0 = d
  | length grid <= i = d
  | length (grid !! i) <= j = d
  | otherwise = grid !! i !! j

neighborCoords :: Coords -> [Coords]
neighborCoords (i, j) =
  let neighborDirections =
        [ (di, dj)
        | let ds = [-1, 0, 1]
        , di <- ds
        , dj <- ds
        , (di, dj) /= (0, 0)
        ]
  in map (\(di, dj) -> (i + di, j + dj)) neighborDirections

-- -- embed a small grid into a big grid at these coords
-- embed :: Grid a -> Grid a -> Coords -> Grid a
-- embed small big at =
--   [
--     [ gridLookupDefault small (gridLookupDefault big ) coords | ] |

--   ]
