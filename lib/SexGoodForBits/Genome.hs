{-# LANGUAGE RecordWildCards #-}

module SexGoodForBits.Genome where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Represent genomes by unboxed vectors of ints
-- restricted to {0, 1, 2, ..., range - 1}
-- E.g. a binary genome would have range = 1.
data Genome = Genome {
                       range :: Int,
                       genes :: U.Vector Int
                     } deriving (Eq, Read, Show)



-- Generate a random genome (from a uniform distribution
-- over possible genomes) of the given range and length.
generateGenome :: Int
               -> Int
               -> Gen RealWorld
               -> IO Genome
generateGenome theRange theLength rng = do
  theGenes <- U.replicateM theLength $ uniformR (0, theRange-1) rng
  return $ Genome theRange theGenes


-- Mutate a genome at a single location
mutate :: Genome
       -> Gen RealWorld
       -> IO Genome
mutate (Genome {..}) rng = do

  -- The location to mutate
  location <- uniformR (0, U.length genes - 1) rng

  -- The new value
  newValue <- uniformR (0, range-1) rng

  -- Put the new value in its place
  genes' <- U.unsafeThaw genes
  _ <- UM.unsafeWrite genes' location newValue
  genes'' <- U.unsafeFreeze genes'

  return $ Genome range genes''

