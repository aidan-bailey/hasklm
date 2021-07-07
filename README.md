# HasKLM

`Hasklm` is a Haskell based implementation of defeasible entailment using the [KLM-approach](https://open.uct.ac.za/handle/11427/32743?show=full).

Checkout the [docs](https://github.com/aidanjbailey/hasklm/tree/master/docs) for all currently available functionality.

[Stack](https://docs.haskellstack.org/en/stable/README/) is the only dependency as it is the project's build tool.

The most commonly used commands are described below.

- Build Package: `stack build`
- Load Library into GHCi: `stack repl`
- Run Tests: `stack test`

It should be noted that I am not yet adept in Haskell stack project management, and as such all library modules are exposed and will be loaded in when calling `stack repl`. This results in a very long GHCi REPL line. I am attempting to fix this such that only the pertinent packages are exposed.

An example of defeasible entailment checking using Rational Closure can be found in the [test file](https://github.com/aidanjbailey/hasklm/blob/master/test/MyLibTest.hs) within the `rationalClosureTest` function.
