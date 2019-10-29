module Conway.Seeds where

import Conway.GameOfLife
import Conway.Serial
import Conway.Seeds.Builders as B

findByName :: String -> Maybe GoL
findByName name
  | name == "glider" = Just gliderWithSpace
  | otherwise = Nothing

glider = GoL(B.glider Dead Live)

gliderString = unlines (B.glider x o)
  where
    x = cellToChar Dead
    o = cellToChar Live

gliderWithSpace = GoL
  [ [x, x, o, x, x, x, x]
  , [o, x, o, x, x, x, x]
  , [x, o, o, x, x, x, x]
  , [x, x, x, x, x, x, x]
  , [x, x, x, x, x, x, x]
  , [x, x, x, x, x, x, x]
  ]
  where
    x = Dead
    o = Live
