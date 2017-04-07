module Main where

-- Imports
import SexGoodForBits.Genome
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Generate and print two genomes
  father <- generateGenome 10 10 rng
  mother <- generateGenome 10 10 rng
  putStrLn $ "Mother = " ++ show mother
  putStrLn $ "Father = " ++ show father

  -- Breed them
  unmutatedChild <- breed mother father rng
  child <- mutate unmutatedChild rng
  putStrLn $ "Child  = " ++ show child

