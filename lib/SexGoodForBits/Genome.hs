module SexGoodForBits.Genome where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
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
  theGenes <- U.replicateM theLength $ uniformR (0, theRange) rng
  return $ Genome theRange theGenes

