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
conway opts = do
  handlePrintHeader opts
  conwayNamedSeed opts
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

conwayNamedSeed :: ConwayOpts -> IO ()
conwayNamedSeed ConwayOpts{seedName=name, numGens=n} =
  case (Seeds.findByName name) of
    Just gol -> dumpGens $ GoL.conwayNGenerations n gol
    Nothing  -> noSuchSeed name

noSuchSeed :: String -> IO ()
noSuchSeed name = puts $ concat
    [ "No such seed:"
    , quote name
    , "!"
    ]

dumpGens :: [GoL.GoL] -> IO ()
dumpGens = sequence_ . (map puts) . (map golToString)
