# hasty-hamiltonian [![Build Status](https://secure.travis-ci.org/jtobin/hasty-hamiltonian.png)](http://travis-ci.org/jtobin/hasty-hamiltonian)

Speedy, gradient-based traversal through parameter space.  See the *examples* folder for example usage.

Install notes:

    cabal sandbox init
    cabal install -j --only-dep

To build the example,

    cabal install -j --only-dep --enable-tests
    cabal build -j hasty-examples

You can then use `./dist/build/hasty-examples/hasty-examples` to see an example trace of a chain wandering over the [Rosenbrock density](http://en.wikipedia.org/wiki/Rosenbrock_function).

