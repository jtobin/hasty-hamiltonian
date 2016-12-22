# hasty-hamiltonian

[![Build Status](https://secure.travis-ci.org/jtobin/hasty-hamiltonian.png)](http://travis-ci.org/jtobin/hasty-hamiltonian)
[![Hackage Version](https://img.shields.io/hackage/v/hasty-hamiltonian.svg)](http://hackage.haskell.org/package/hasty-hamiltonian)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/hasty-hamiltonian/blob/master/LICENSE)


Speedy, gradient-based traversal through parameter space.

Exports a `mcmc` function that prints a trace to stdout, a `chain` function for
collecting results in memory, and a `hamiltonian` transition operator that can
be used more generally.

If you don't want to calculate your gradients by hand you can use the handy
[ad](https://hackage.haskell.org/package/ad) library for automatic
differentiation.

    import Numeric.AD (grad)
    import Numeric.MCMC.Hamiltonian

    target :: RealFloat a => [a] -> a
    target [x0, x1] = negate ((x0 + 2 * x1 - 7) ^ 2 + (2 * x0 + x1 - 5) ^ 2)

    gTarget :: [Double] -> [Double]
    gTarget = grad target

    booth :: Target [Double]
    booth = Target target (Just gTarget)

    main :: IO ()
    main = withSystemRandom . asGenIO $ mcmc 10000 0.05 20 [0, 0] booth

![trace](https://dl.dropboxusercontent.com/spa/u0s6617yxinm2ca/h6ty39dl.png)

*hasty-hamiltonian* is a member of the [declarative][decl] suite of libraries,
containing a bunch of MCMC algorithms that play nicely together.

[decl]: https://github.com/jtobin/declarative
