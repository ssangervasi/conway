module Main (main) where

import Options.Applicative
import Data.Semigroup ( (<>) )

import PrintHelpers
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
  , numGens :: Int
  , printHeader :: Bool
  }

conwayOpts :: Parser ConwayOpts
conwayOpts =
  ConwayOpts
    <$> strOption
          (  long "seed-name"
          <> short 's'
          <> metavar "SEED_NAME"
          <> help  "The name of the game seed" 
          )
    <*> option auto
          (  long "num-gens"
          <> short 'n'
          <> help "The number of generations to run for"
          <> showDefault
          <> value 10
          <> metavar "NUM_GENS"
          )
    <*> switch
          (  long "print-header"
          <> short 'H'
          <> help "Whether to print a header before the results."
          )


conway :: ConwayOpts -> IO ()
conway opts@ConwayOpts{seedName=s, numGens=n} = do
  handlePrintHeader opts
  conwayNamedSeed s n
  return ()

handlePrintHeader :: ConwayOpts -> IO ()
handlePrintHeader ConwayOpts
  { printHeader=shouldPrint
  , seedName=s
  , numGens=n
  }
  | shouldPrint = puts formatHeader
  | otherwise = return ()
  where
    formatHeader = linesOfWords
      [ ["Conway's Game of Life"]
      , ["Starting with seed:", quote s]
      , ["Number of generations:", show n]
      ]

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
  let fiveGens = GoL.conwayNGenerations 15 Seeds.gliderWithSpace
  sequence_ $ map puts $ map golToString fiveGens
