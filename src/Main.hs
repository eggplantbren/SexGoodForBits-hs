module Main where

-- Imports
import SexGoodForBits.Genome
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Generate and print a genome.
  genome <- generateGenome 10 10 rng
  print genome

  -- Mutate it
  genome' <- mutate genome rng
  print genome'


