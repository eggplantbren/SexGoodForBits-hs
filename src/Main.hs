module Main where

-- Imports
import SexGoodForBits.Genome
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
  -- Generate and print a genome.
  genome <- generateGenome 1 10 rng
  print genome


