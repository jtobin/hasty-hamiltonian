{-# OPTIONS_GHC -Wall #-}

-- | Hamiltonian Monte Carlo.  See, ex: Neal (2012)
--   http://arxiv.org/pdf/1206.1901.pdf.

module Numeric.MCMC.Hamiltonian (
    Target(..)
  , Tunables(..)
  , Options(..)
  , hamiltonian
  , hmc
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions


-- | The target we want to sample from.  Consists of a log target and its
--   gradient.
data Target = Target {
    lTarget  :: Parameters -> Double     -- ^ log target
  , glTarget :: Parameters -> Parameters -- ^ gradient of log target
  }

type Parameters = Vector Double

data Tunables = Tunables {
    stepSize  :: !Double -- ^ step size for a given proposal
  , leapfrogs :: !Int    -- ^ number of leapfrog steps to take
  } deriving Show

data Options = Options {
    epochs  :: !Int        -- ^ number of epochs to iterate the chain
  , initial :: !Parameters -- ^ start location
  } deriving Show

type Particle = (Parameters, Parameters)

type Transition m = StateT Parameters m Parameters

-- | The Hamiltonian transition operator.
hamiltonian
  :: PrimMonad m
  => Target
  -> Tunables
  -> Gen (PrimState m)
  -> Transition m
hamiltonian target tunables g = do
  q0 <- get
  r0 <- V.replicateM (V.length q0) (lift $ standard g)
  zc <- lift $ uniform g
  let (q, r) = leapfrogIntegrator target tunables (q0, r0)
      next   = nextState target (q0, q) (r0, r) zc
  put next
  return next

-- | The leapfrog or Stormer-Verlet integrator.
leapfrogIntegrator :: Target -> Tunables -> Particle -> Particle
leapfrogIntegrator target tunables (q0, r0) = go q0 r0 l where
  l        = leapfrogs tunables
  go q r 0 = (q, r)
  go q r n =
    let (q1, r1) = leapfrog target tunables (q, r)
    in  go q1 r1 (pred n)

-- | A single iteration of the leapfrog integrator.
leapfrog :: Target -> Tunables -> Particle -> Particle
leapfrog target tunables (q, r) = (qf, rf) where
  rm = adjustMomentum target tunables (q, r)
  qf = adjustPosition tunables (rm, q)
  rf = adjustMomentum target tunables (qf, rm)

-- | Adjust momentum according to a half-leapfrog step.
adjustMomentum :: Target -> Tunables -> Particle -> Parameters
adjustMomentum target tunables (q, r) = r .+ ((0.5 * e) .* g q) where
  e = stepSize tunables
  g = glTarget target

-- | Adjust position according to a half-leapfrog step.
adjustPosition :: Tunables -> Particle -> Parameters
adjustPosition tunables (r, q) = q .+ (e .* r) where
  e = stepSize tunables

-- | Scalar-vector multiplication.
(.*) :: Double -> Parameters -> Parameters
z .* xs = (* z) <$> xs

-- | Scalar-vector addition.
(.+) :: Parameters -> Parameters -> Parameters
xs .+ ys = V.zipWith (+) xs ys

-- | The next state of the Markov chain.
nextState
  :: Target
  -> Particle
  -> Particle
  -> Double
  -> Parameters
nextState target position momentum z
    | z < pAccept = snd position
    | otherwise   = fst position
  where
    pAccept = acceptProb target position momentum

-- | A target augmented by momentum auxilliary variables.
auxilliaryTarget :: Target -> Particle -> Double
auxilliaryTarget target (t, r) = f t - 0.5 * innerProduct r r where
  f = lTarget target

-- | The acceptance probability of a move.
acceptProb :: Target -> Particle -> Particle -> Double
acceptProb target (q0, q1) (r0, r1) = exp . min 0 $
  auxilliaryTarget target (q1, r1) - auxilliaryTarget target (q0, r0)

-- | Simple inner product.
innerProduct :: Parameters -> Parameters -> Double
innerProduct xs ys = V.sum $ V.zipWith (*) xs ys

-- | Run a chain of HMC.
hmc
  :: PrimMonad m
  => Target
  -> Tunables
  -> Options
  -> Gen (PrimState m)
  -> m [Parameters]
hmc target tunables options g = do
  let h = hamiltonian target tunables g
      n = epochs options
      q = initial options
  evalStateT (replicateM n h) q

