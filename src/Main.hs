module Main where

-- Imports
import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import SexGoodForBits.Genome
import SexGoodForBits.Population
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Create a population
  population <- generatePopulation 1000 (2, 1000) rng

  -- Define a fitness function
  let fitnessFunction (Genome _ gs) = fromIntegral (U.sum gs)

  -- Evaluate fitnesses
  let fs      = fitnesses fitnessFunction population
      fsList  = V.toList fs
      sorted  = sort fsList
      strings = map (\s -> show s ++ "\n") sorted
      string  = mconcat strings

  putStrLn string
  k <- choose fs rng
  print (fs V.! k)

  return ()

