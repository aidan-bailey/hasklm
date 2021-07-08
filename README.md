# HasKLM

`Hasklm` is a Haskell based implementation of defeasible entailment checking using the [KLM-approach](https://open.uct.ac.za/handle/11427/32743?show=full).

Checkout the [docs](https://github.com/aidanjbailey/hasklm/tree/master/docs) for all currently available functionality.

[Stack](https://docs.haskellstack.org/en/stable/README/) is the only dependency as it is the project's build tool.

The most commonly used commands are described below.

- Build Package: `stack build`
- Load Library into GHCi (For Development/Testing purposes): `stack repl`
- Run Tests: `stack test`
- Use Application: `stack repl --only-main`

## Examples

Examples of defeasible entailment checking, using Rational Closure, can be found in the [test file](https://github.com/aidanjbailey/hasklm/blob/master/test/MyLibTest.hs).

### RationalClosure Example

This example will be in reference to the [birds and penguins](https://projects.cs.uct.ac.za/honsproj/cgi-bin/view/2019/morris_ross.zip/images/comic-penguins-strip.png) classical reasoning problem.

1. Load the Application.

```sh
stack repl --only-main
```

2. Define the defeasible relations.

```haskell
let defRel1 = typically (str2form "bird Implies flies")
let defRel2 = typically (str2form "bird Implies wings")
let defRel3 = typically (str2form "penguin Implies Not flies")
```

3. Construct a defeasible knowledge base using the defeasible relations.

```haskell
let dkb = [defRel1, defRel2, defRel3]
```

4. Define the classical propositional statements.

```haskell
let propForm1 = str2form "penguin Implies bird"
let propForm2 = str2form "Robin Implies bird"
```

5. Construct the propositional knowledge base.

```haskell
let pkb = [propForm1, propForm2]
```

6. Combine the propositional and defeasible knowledge bases into a joint knowledge base relation.

```haskell
let jkb = (pkb, dkb)
```

7. Construct a defeasible query.

```haskell
let defQuery = typically (str2form "penguin Implies bird")
```

8. Execute Rational Closure with the joint knowledge base and defeasible query and print the result.

```haskell
print (entailsRC jkb defQuery)
```

9. If the above outputs `True`, all is well! A penguin is still a bird! For interests sake, we will define another defeasible query.

```haskell
let defQuery2 = typically (str2form "penguin Implies wings")
```

10. Execute and print as before.

```haskell
print (entailsRC jkb defQuery2)
```

11. If the above outputs `False`, all is still well! Rational Closure would conclude through the some process that since penguins are atypical birds that do not fly, penguins should not have wings either.
