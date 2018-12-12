module Conway.Seeds where

import Conway.GameOfLife

glider = GoL [
    [Dead, Dead, Live],
    [Live, Dead, Live],
    [Dead, Live, Live]
  ]

gliderString = unlines [
    "⬜️⬜️🔳",
    "🔳⬜️🔳",
    "⬜️🔳🔳"
  ]
