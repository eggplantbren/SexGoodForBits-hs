module SexGoodForBits.Population where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import SexGoodForBits.Genome
import System.Random.MWC

-- Type for a Population
data Population = Population { populationAge :: Int,
                               populationGenomes :: V.Vector Genome }
    deriving (Eq, Read, Show)

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
          -> V.Vector Double
fitnesses fitnessFunction (Population _ gs) = V.map fitnessFunction gs

-- Choose a genome from the population, with
-- more probability for selecting good ones.
choose :: V.Vector Double       -- Vector of fitnesses
       -> Gen RealWorld
       -> IO Int
choose fs rng = do
  let n = V.length fs
  k <- uniformR (0, n-1) rng :: IO Int

  -- Does the k'th fitness beat each other one?
  let beaten = V.map
               (\f -> if (fs V.! k) >= f then 1 else 0)
               fs :: V.Vector Int

  let pAccept = fromIntegral (V.sum beaten) / (fromIntegral n)
  u <- uniform rng :: IO Double

  if u < pAccept then return k else choose fs rng

---- Update population
--update :: Population -> Gen RealWorld -> IO Population
--update population rng = do
--  

