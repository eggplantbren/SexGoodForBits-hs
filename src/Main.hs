module Main where

-- Imports
import SexGoodForBits.Population
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Create a population
  population <- generatePopulation 1000 (2, 1000) rng

  return ()

