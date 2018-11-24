module Conway.Glider
  ( runGlider
  )
where

import           Conway.GameOfLife

runGlider :: IO ()
runGlider = do
  putStrLn "Conway's Game of Life"
  putStrLn "Starting with seed: 'Glider'"
  let fiveGens = conwayNGenerations 15 glider
  sequence_ $ map putStrLn $ map show fiveGens


glider :: GoL
glider = GoL
  [ [Dead, Dead, Live, Dead, Dead, Dead, Dead]
  , [Live, Dead, Live, Dead, Dead, Dead, Dead]
  , [Dead, Live, Live, Dead, Dead, Dead, Dead]
  , [Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  , [Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  , [Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  ]
