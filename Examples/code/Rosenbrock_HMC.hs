import Numeric.MCMC.Hamiltonian
import System.Random.MWC
import System.Environment
import System.Exit
import System.IO
import Numeric.AD
import Control.Monad

target :: RealFloat a => [a] -> a
target [x0, x1] = (-1)*(5*(x1 - x0^2)^2 + 0.05*(1 - x0)^2)

gTarget :: [Double] -> [Double]
gTarget = grad target

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(hasty-hamiltonian) Rosenbrock density                       "
        putStrLn  "Usage: ./Rosenbrock_HMC <numSteps> <nDisc> <stepSize> <inits>" 
        putStrLn  "                                                             "
        putStrLn  "numSteps         : Number of Markov chain iterations to run. "
        putStrLn  "nDisc            : Number of discretizing steps to take.     "
        putStrLn  "stepSize         : Perturbation scaling parameter.           "
        putStrLn  "inits            : Filepath containing points at which to    "
        putStrLn  "                   initialize the chain.                     "
        exitSuccess

    inits <- fmap (map read . words) (readFile (args !! 3)) :: IO [Double]

    let nepochs = read (head args) :: Int
        nDisc   = read (args !! 1) :: Double
        eps     = read (args !! 2) :: Double
        params  = Options target gTarget nDisc eps
        config  = MarkovChain inits 0

    g       <- create
    results <- runChain params nepochs config g

    hPutStrLn stderr $ 
        let nAcc  = accepts results
            total = nepochs 
        in  show nAcc ++ " / " ++ show total ++ " (" ++ 
              show ((fromIntegral nAcc / fromIntegral total) :: Float) ++ 
              ") proposals accepted"

