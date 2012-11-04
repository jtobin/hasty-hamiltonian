{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Numeric.MCMC.Hamiltonian where

import Control.Monad.Reader
import Control.Monad.Primitive
import System.Random.MWC
import System.Random.MWC.Distributions

-- | State of the Markov chain.  Current parameter values are held in 'theta', 
--   while accepts counts the number of proposals accepted.
data MarkovChain = MarkovChain { theta   :: [Double] 
                               , accepts :: {-# UNPACK #-} !Int }

-- | Options for the chain.  The target (expected to be a log density), its 
--   gradient, and a step size tuning parameter.
data Options = Options { _target  :: [Double] -> Double
                       , _gTarget :: [Double] -> [Double]
                       , _nLeaps  :: {-# UNPACK #-} !Double  
                       , _eps     :: {-# UNPACK #-} !Double }

-- | A result with this type has a view of the chain options.
type ViewsOptions = ReaderT Options

-- | Display the current state. 
instance Show MarkovChain where
    show config = filter (`notElem` "[]") $ show (theta config)

-- | The 'leapfrog' or Stormer-Verlet discretizer.
leapfrog :: Monad m 
         => [Double] 
         -> [Double] 
         -> ViewsOptions m ([Double], [Double])
leapfrog t0 r0 = do
    Options _ gTarget ndisc e <- ask
    let go !t !r n 
            | n == 0    = (t, r)
            | otherwise = let rm = zipWith (+) r  (map (* (0.5*e)) (gTarget t))
                              tt = zipWith (+) t  (map (*e) rm)
                              rt = zipWith (+) rm (map (* (0.5*e)) (gTarget tt))
                          in  go tt rt (n - 1)
    return $! go t0 r0 ndisc 
{-# INLINE leapfrog #-}

-- | Perform a Metropolis accept/reject step.
metropolisStep :: PrimMonad m 
               => MarkovChain 
               -> Gen (PrimState m) 
               -> ViewsOptions m MarkovChain
metropolisStep state g = do
    Options target _ _ _ <- ask
    let t0   = theta state
        nacc = accepts state
    r0     <- replicateM (length t0) (lift $ standard g)
    zc     <- lift $ uniformR (0 :: Double, 1 :: Double) g
    (t, r) <- leapfrog t0 r0

    let mc = if   zc < min 1 (exp arRatio)
             then (t,  1)
             else (t0, 0)
        
        xs <.> ys = sum $ zipWith (*) xs ys
        arRatio   =   target t  + 0.5*(r0 <.> r0)
                    - target t0 - 0.5*(r <.> r)

    return $! MarkovChain (fst mc) (nacc + snd mc)
{-# INLINE metropolisStep #-}
 
-- | Diffuse through states.
runChain :: Options         -- Options of the Markov chain.
         -> Int             -- Number of epochs to iterate the chain.
         -> Int             -- Print every nth iteration.
         -> MarkovChain     -- Initial state of the Markov chain.
         -> Gen RealWorld   -- MWC PRNG
         -> IO MarkovChain  -- End state of the Markov chain, wrapped in IO.
runChain = go 
  where go o n t !c g | n == 0 = return c
                      | n `rem` t /= 0 = do
                            r <- runReaderT (metropolisStep c g) o
                            go o (n - 1) t r g
                      | otherwise = do
                            r <- runReaderT (metropolisStep c g) o
                            print r
                            go o (n - 1) t r g
{-# INLINE runChain #-}

