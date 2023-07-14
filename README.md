# Deftab, a tableau-based modal prover with defaults

Deftab implements a tableau calculus to handle
modal logics with defaults, namely:

* hybrid default logic
* intuitionistic default logic

It handles sceptical consequence check.

Deftab supports normal default rules only
(rules where the justification is equal to the consequent).

## How to compile

You need:

* ghc 8.4 or newer
* cabal-install 2.4 or newer
* alex
* happy

Compilation steps:

* go to the directory that contains the file `deftab.cabal`
* run `cabal install` and wait

## How to run

Where is the binary:

* the `deftab` executable will be copied to the default Cabal
  binary folder, which is `~/.cabal/bin` under Linux.
* it is also available from the current directory (where
  `deftab.cabal` can be found) at path `./dist/build/deftab/deftab`.

To get help about `deftab`'s options run: `deftab --help`.

The simplest way to see `deftab` run with an example formula is,
for instance:

* `deftab -f ./examples/theorem.01.dt`
