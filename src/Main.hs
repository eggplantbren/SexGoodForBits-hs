module Main where

-- Imports
import Data.Vector.Unboxed as U
import SexGoodForBits.Genome
import SexGoodForBits.Population
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Define a fitness function
  let fitnessFunction (Genome _ gs) = fromIntegral (U.sum gs)

  -- Create a population
  population <- generatePopulation 1000 (2, 1000) rng

  -- Update it 200 times
  _ <- updateManyTimes 200 population fitnessFunction rng

  return ()

