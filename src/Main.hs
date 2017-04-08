module Main where

-- Imports
import SexGoodForBits.Examples
import SexGoodForBits.Population
import System.Random.MWC

-- Main action
main :: IO ()
main = do

  -- Setup RNG with fixed seed
  rng <- System.Random.MWC.create

  -- Create a population
  initialPop <- generatePopulation 1000 (2, 10) rng

  -- Update it 200 times
  _ <- updateManyTimes 200 initialPop simple rng

  return ()

