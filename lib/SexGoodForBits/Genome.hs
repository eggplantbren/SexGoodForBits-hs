{-# LANGUAGE RecordWildCards #-}

module SexGoodForBits.Genome where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Represent genomes by unboxed vectors of ints
-- restricted to {0, 1, 2, ..., range - 1}
-- E.g. a binary genome would have range = 2.
data Genome = Genome {
                       range :: {-# UNPACK #-} !Int,
                       genes :: U.Vector Int
                     } deriving (Eq, Read, Show)

-- Generate a random genome (from a uniform distribution
-- over possible genomes) of the given range and length.
generateGenome :: Int
               -> Int
               -> Gen RealWorld
               -> IO Genome
generateGenome theRange theLength rng = do
  let rand = uniformR (0, theRange - 1) rng :: IO Int
  theGenes <- U.replicateM theLength rand
  return $ Genome theRange theGenes


-- Mutate a genome at a single location
mutate :: Genome
       -> Gen RealWorld
       -> IO Genome
mutate Genome {..} rng = do

  -- The location to mutate
  location <- uniformR (0, U.length genes - 1) rng

  -- The new value
  newValue <- uniformR (0, range-1) rng

  -- Put the new value in its place
  genes' <- U.unsafeThaw genes
  _ <- UM.unsafeWrite genes' location newValue
  genes'' <- U.unsafeFreeze genes'

  return $ Genome range genes''


-- Crossover of two genomes to create an un-mutated child
crossover :: Genome -> Genome
          -> Gen RealWorld
          -> IO Genome
crossover (Genome range1 genes1) (Genome range2 genes2) rng = do

      -- A function in IO that chooses either x or y
      -- depending on the outcome of a random bit
      let pick x y = do
                       choice <- uniformR (0, 1) rng :: IO Int
                       let result = if choice == 0 then x else y
                       return result

      -- The child's genes
      childGenes <- U.zipWithM pick genes1 genes2

      let childRange = maximum [range1, range2]
      let child = Genome childRange childGenes
      return child


-- Breed two genomes
breed :: Genome -> Genome -> Gen RealWorld -> IO Genome
breed mother father rng =
  crossover mother father rng >>= (`mutate` rng)

