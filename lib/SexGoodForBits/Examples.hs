-- Some example fitness functions
module SexGoodForBits.Examples where

-- Imports
import qualified Data.Vector.Unboxed as U
import SexGoodForBits.Genome

-- A very simple example fitness function
simple :: Genome -> Double
simple (Genome _ gs) = fromIntegral (U.sum gs)

-- Rosenbrock example, as in DNest4 and NestedSampling-hs
rosenbrock :: Genome -> Double
rosenbrock (Genome theRange gs) = let
    r   = fromIntegral theRange :: Double
    a   = 0.5 / r
    b   = 1.0 / r
    us  = U.map ((+a) . (*b) . fromIntegral) gs -- \in (0, 1)
    xs  = U.map (\u -> -10.0 + 20.0*u) us
    f   = (sum terms1) + (sum terms2)
    terms1 = [100.0*(xs U.! (i+1) - (xs U.! i)**2)**2 | i <- [0..(n-2)]]
    terms2 = [(1.0 - (xs U.! i))**2 | i <- [0..(n-2)]]
    n      = U.length xs :: Int
  in
    (-2.0) * f

