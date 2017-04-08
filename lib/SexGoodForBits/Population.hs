{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module SexGoodForBits.Population where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import SexGoodForBits.Genome
import System.Random.MWC

-- Type for a Population
data Population = Population { populationAge     :: {-# UNPACK #-} !Int,
                               populationGenomes :: V.Vector Genome }
    deriving (Eq, Read, Show)

-- Print some stats
printStats :: Population -> U.Vector Double -> IO ()
printStats Population {..} fs = do
  let meanFitness = U.sum fs / fromIntegral (U.length fs)
      maxFitness  = U.maximum fs
  putStr $ "Population age = " ++ show populationAge ++ ", "
  putStr $ "mean fitness = " ++ show meanFitness ++ ", "
  putStrLn $ "max fitness = " ++ show maxFitness

-- Generate an initial population
generatePopulation :: Int
                   -> (Int, Int)
                   -> Gen RealWorld
                   -> IO Population
generatePopulation populationSize (genomeRange, genomeLength) rng = do
  let generateOne = generateGenome genomeRange genomeLength rng
  genomes <- V.replicateM populationSize generateOne
  return $ Population 0 genomes

-- Evaluate given fitness function for each genome
fitnesses :: (Genome -> Double)
          -> Population
          -> U.Vector Double
fitnesses fitnessFunction (Population _ gs) = let
    fs = V.map fitnessFunction gs
  in
    V.convert fs :: U.Vector Double

-- Choose a genome from the population, with
-- more probability for selecting good ones.
choose :: U.Vector Double       -- Vector of fitnesses
       -> Gen RealWorld
       -> IO Int
choose fs rng = do
  let n = U.length fs
  k <- uniformR (0, n-1) rng :: IO Int

  -- Does the k'th fitness beat each other one?
  let beaten = U.map
               (\f -> if (fs U.! k) >= f then 1 else 0)
               fs :: U.Vector Int

  let pAccept = fromIntegral (U.sum beaten) / fromIntegral n
  u <- uniform rng :: IO Double

  if u < pAccept then return k else choose fs rng

-- Generate an offspring from an existing population
-- whose fitnesses are also provided.
generateOffspring :: Population
                  -> U.Vector Double
                  -> Gen RealWorld
                  -> IO Genome
generateOffspring (Population _ genomes) fs rng = do
  -- Choose the mother and father
  i <- choose fs rng
  j <- choose fs rng
  breed (genomes V.! i) (genomes V.! j) rng

-- Update population
update :: Population
       -> (Genome -> Double)
       -> Gen RealWorld
       -> IO Population
update pop@Population {..} fitnessFunction rng = do
  let n  = V.length populationGenomes
      fs = fitnesses fitnessFunction pop

  printStats pop fs

  newGenomes <- V.replicateM n (generateOffspring pop fs rng)
  let !newPop = Population (populationAge + 1) newGenomes
  return newPop

-- Update population many times
updateManyTimes :: Int
                -> Population
                -> (Genome -> Double)
                -> Gen RealWorld
                -> IO Population
updateManyTimes n pop fitnessFunction rng
  | n == 0    = do
                  printStats pop (fitnesses fitnessFunction pop)
                  return pop
  | otherwise = do
                  newPop <- update pop fitnessFunction rng
                  updateManyTimes (n-1) newPop fitnessFunction rng

