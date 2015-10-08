{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Numeric.AD (grad)
import Numeric.MCMC.Hamiltonian

target :: RealFloat a => [a] -> a
target [x0, x1]
  | and [x0 >= -4.5, x0 <= 4.5, x1 >= -4.5, x1 <= 4.5]
      = negate $ (1.5   - x0 + x0 * x1) ^ 2
               + (2.25  - x0 + x0 * x1 ^ 2) ^ 2
               + (2.625 - x0 + x0 * x1 ^ 3) ^ 2
  | otherwise = - (1 / 0)

gTarget :: [Double] -> [Double]
gTarget = grad target

beale :: Target [Double]
beale = Target target (Just gTarget)

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 10000 0.05 20 [0, 0] beale


