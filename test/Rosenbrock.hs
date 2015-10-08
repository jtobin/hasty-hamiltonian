{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Numeric.AD (grad)
import Numeric.MCMC.Hamiltonian

target :: RealFloat a => [a] -> a
target [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

gTarget :: [Double] -> [Double]
gTarget = grad target

rosenbrock :: Target [Double]
rosenbrock = Target target (Just gTarget)

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 3000 0.05 20 [0, 0] rosenbrock

