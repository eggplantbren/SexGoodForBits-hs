module SexGoodForBits.Population where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import SexGoodForBits.Genome
import System.Random.MWC

-- Type for a Population
data Population = Population (V.Vector Genome)
    deriving (Eq, Read, Show)

-- Generate an initial population
generatePopulation :: Int
                   -> (Int, Int)
                   -> Gen RealWorld
                   -> IO Population
generatePopulation populationSize (genomeRange, genomeLength) rng = do
  let generateOne = generateGenome genomeRange genomeLength rng
  genomes <- V.replicateM populationSize generateOne
  return $ Population genomes

