# HasKLM

`Hasklm` is a Haskell based defeasible reasoner using the [KLM-approach](https://open.uct.ac.za/handle/11427/32743?show=full).

## Features

**Overview Feature List:**

- Parsing a string into a propositional formula.
- Computation of the valuations/models for propositional knowledge bases.
- Propositional satisfiability checking.
- Propositional entailment checking.
- The Base Rank algorithm.
- Rational Closure based defeasible entailment checking.

**Planned Feature List:**

- Lexicographic Closure based defeasible entailment checking.
- Rigorous testing.
- The Relevant Closure algorithm.
- Propositional knowledge base parsing.
- Defeasible implication parsing.
- Defeasible knowledge base parsing.
- Joint knowledge base parsing.
- Knowledge base generation.

Checkout the [docs](https://github.com/aidanjbailey/hasklm/tree/master/docs) for all currently available functionality.

## Usage

[Stack](https://docs.haskellstack.org/en/stable/README/) is the project's build tool.

**Commonly Used Commands:**

Build Project:

```sh
stack build
```

Run Tests:

```sh
stack test
```

Load All Modules into GHCi (For Development/Testing purposes):

```sh
stack repl
```

Load Application into GHCi:

```sh
stack repl --only-main
```

## Examples

Examples can be found in the [test file](https://github.com/aidanjbailey/hasklm/blob/master/test/MyLibTest.hs).

### RationalClosure Example

This example will be in reference to the [birds and penguins](https://projects.cs.uct.ac.za/honsproj/cgi-bin/view/2019/morris_ross.zip/images/comic-penguins-strip.png) classical reasoning problem.

**Instructions**

Load the application.

```sh
stack repl --only-main
```

Define the _defeasible implications_.

```haskell
let defRel1 = typically (str2form "bird Implies flies")
let defRel2 = typically (str2form "bird Implies wings")
let defRel3 = typically (str2form "penguin Implies Not flies")
```

Construct a _defeasible knowledge base_ using the defeasible implications.

```haskell
let dkb = [defRel1, defRel2, defRel3]
```

Define the classical _propositional formulas_.

```haskell
let propForm1 = str2form "penguin Implies bird"
let propForm2 = str2form "Robin Implies bird"
```

Define the _propositional knowledge base_.

```haskell
let pkb = [propForm1, propForm2]
```

Combine the propositional and defeasible knowledge bases into a _joint knowledge base relation_.

```haskell
let jkb = (pkb, dkb)
```

Define a _defeasible query_.

```haskell
let defQuery = typically (str2form "penguin Implies bird")
```

Execute _Rational Closure defeasible entailment checking_ with the joint knowledge base and defeasible query, printing the result.

```haskell
print (entailsRC jkb defQuery)
```

If the above outputs `True`, all is well! A penguin is still a bird! For interests sake, we will define another defeasible query.

```haskell
let defQuery2 = typically (str2form "penguin Implies wings")
```

Execute and print as before.

```haskell
print (entailsRC jkb defQuery2)
```

If the above outputs `False`, all is still well! Rational Closure concludes that since penguins are atypical birds that do not fly, it is most likely that penguins do not have wings either.

## Disclaimer

I am still quite a novice when it comes to formalised logic.
I've tried to define everything using names that closely relate to their corresponding concepts in formal KRR literature, mostly in reference to [Adam Kaliski's 2020 Dissertation](https://open.uct.ac.za/handle/11427/32743?show=full) on the topic.
Some (if not most) of the functions and procedures are not as efficient as they could be, but that's just the nature of programming isn't it.

With that being said, I am open to any and all suggestions in order to improve the repo, whether it's in readability, efficiency, functionality, Haskell best practices, or just to give it better relations to the literature.
