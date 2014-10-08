{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable (toList)
import Data.Vector (Vector, fromList)
import Numeric.AD
import Numeric.MCMC.Hamiltonian
import System.Random.MWC

lRosenbrock :: RealFloat a => Vector a -> a
lRosenbrock xs = negate $ 5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2 where
  [x0, x1] = toList xs

glRosenbrock :: Vector Double -> Vector Double
glRosenbrock = grad lRosenbrock

rosenbrock :: Target
rosenbrock = Target lRosenbrock glRosenbrock

tunables :: Tunables
tunables = Tunables 0.05 20

options :: Options
options = Options 5000 (fromList [1, 1])

main :: IO ()
main = do
  g     <- create
  trace <- hmc rosenbrock tunables options g
  mapM_ print trace

