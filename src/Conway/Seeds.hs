module Conway.Seeds where

import Conway.GameOfLife

glider = GoL [
    [x, x, o],
    [o, x, o],
    [x, o, o]
  ]
  where x = Dead
        o = Live

gliderString = unlines [
    [x, x, o],
    [o, x, o],
    [x, o, o]
  ]
  where
    -- Emoji ⬜️
    x = '\128307'   
    -- Emoji 🔳
    o = '\11036'
