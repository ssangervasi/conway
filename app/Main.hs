module Main (main) where

import Options.Applicative
import Data.Semigroup ( (<>) )

import Puts
import qualified Conway.GameOfLife as GoL
import qualified Conway.Seeds as Seeds
import Conway.Serial (golToString)

main :: IO ()
main = conway =<< execParser opts
 where
  opts = info
    (conwayOpts <**> helper)
    (fullDesc
    <> progDesc "Conway's Game of Life"
    <> header "Run for NUM_GENS generations starting with SEED_NAME"
    )

data ConwayOpts = ConwayOpts
  { seedName :: String
  , numGens :: Int }

conwayOpts :: Parser ConwayOpts
conwayOpts =
  ConwayOpts
    <$> strOption
          (long "seed-name"
          <> short 's'
          <> metavar "SEED_NAME"
          <> help  "The name of the game seed"
          )
    <*> option
          auto
          (  long "num-gens"
          <> short 'n'
          <> help "The number of generations to run for"
          <> showDefault
          <> value 10
          <> metavar "NUM_GENS"
          )

conway :: ConwayOpts -> IO ()
conway (ConwayOpts s n) = do
  puts $ unwords [
       "Running"
      , s
      , "for"
      , show n
      , "generations."
    ]
  conwayNamedSeed s n
  return ()


conwayNamedSeed :: String -> Int -> IO ()
conwayNamedSeed s _
  | s == "glider" = runGlider
  | otherwise     = noSuchSeed s

noSuchSeed :: String -> IO ()
noSuchSeed s = puts $ concat [
      "No such seed: '"
    , s
    , "'!"
  ]

runGlider :: IO ()
runGlider = do
  puts "Conway's Game of Life"
  puts "Starting with seed: 'Glider'"
  let fiveGens = GoL.conwayNGenerations 15 Seeds.gliderWithSpace
  sequence_ $ map puts $ map golToString fiveGens
