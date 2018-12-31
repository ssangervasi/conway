module Conway.Seeds where

import Conway.GameOfLife
import Conway.Serial

findByName :: String -> Maybe GoL 
findByName name
  | name == "glider" = Just gliderWithSpace
  | otherwise = Nothing

glider = GoL
  [ [x, x, o]
  , [o, x, o]
  , [x, o, o]
  ]
  where
    x = Dead
    o = Live

gliderString = unlines
  [ [x, x, o]
  , [o, x, o]
  , [x, o, o]
  ]
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
