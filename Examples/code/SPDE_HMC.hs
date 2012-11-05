{-# LANGUAGE BangPatterns #-}

import Numeric.MCMC.Hamiltonian
import System.Random.MWC
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Numeric.AD

target :: RealFloat a => [a] -> a
target xs = go 0 0 xs 
  where go !t0 !t1 []         = (- t0 / (2*h)) - (0.5 * h * t1)
        go !t0 !t1 (u:us:uss) = go (t0 + (us - u)^2) (t1 + v (us + u)) uss
        h   = 1 / fromIntegral (length xs)
        v x = (1 - x^2)^2
{-# INLINE target #-}

gTarget :: [Double] -> [Double]
gTarget = grad target
{-# INLINE gTarget #-}

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(hasty-hamiltonian) Stochastic partial differential equation "
        putStrLn  "Usage: ./SPDE_HMC <numSteps> <inits> <thinEvery>             " 
        putStrLn  "                                                             "
        putStrLn  "numSteps         : Number of Markov chain iterations to run.             "
        putStrLn  "thinEvery        : Print every nth iteration.                            "
        putStrLn  "nDisc            : Number of discretizing steps to take.                 "
        putStrLn  "stepSize         : Perturbation scaling parameter.                       "
        putStrLn  "inits            : Filepath containing points at which to                "
        putStrLn  "                   initialize the chain.                                 "
        exitSuccess

    inits <- fmap (map read . words) (readFile (args !! 4)) :: IO [Double]

    let nepochs   = read (head args) :: Int
        thinEvery = read (args !! 1) :: Int
        nDisc     = read (args !! 2) :: Double
        eps       = read (args !! 3) :: Double
        params    = Options target gTarget nDisc eps
        config    = MarkovChain inits 0

    g       <- create
    results <- runChain params nepochs thinEvery config g

    hPutStrLn stderr $ 
        let nAcc  = accepts results
            total = nepochs 
        in  show nAcc ++ " / " ++ show total ++ " (" ++ 
              show ((fromIntegral nAcc / fromIntegral total) :: Float) ++ 
              ") proposals accepted"

