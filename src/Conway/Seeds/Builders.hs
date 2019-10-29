module Conway.Seeds.Builders where

import Conway.Grid

glider :: a -> a -> Grid a
glider x o =
  [ [x, x, o]
  , [o, x, o]
  , [x, o, o]
  ]
