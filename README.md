# `plutarch-quickcheck`

## What is this?

A helper library to write Plutarch-oriented QuickCheck property tests. It
includes support for both 'general' QuickCheck-based property testing and testing (some) Plutarch type class laws. These helpers
integrate seamlessly with
[`tasty-quickcheck`](https://hackage.haskell.org/package/tasty-quickcheck).

## What _exactly_ does this do for me?

QuickCheck is a folkloric minefield and can be quite tedious to use correctly.
Furthermore, its interactions with Plutarch can be non-obvious, and much
of the built-in functionality it provides is not very useful for it.
Furthermore, while Plutarch type classes do have laws, verifying them by hand
can be complex and error-prone. Lastly, the error output of QuickCheck
(and, by extension, `tasty-quickcheck`) can be hard to understand.

`plutarch-quickcheck` aims to fix all of these issues:

* As long as you can provide generators and shrinkers, you don't have to
  interact with any other part of QuickCheck _at all_; this library handles the 
  boilerplate. This applies equally to both general properties and laws checks.
* Issues of QuickCheck coverage, especially for conditional properties, are
  handled automatically: you never have to consider this.
* Whenever possible, `plutarch-quickcheck` will ensure that things are defined,
  and behaving sensibly relative to one another, and will inform you (quite
  loudly) if not.
* Error output tries to be as helpful as it can, both by using prettyprinting
  and also explaining in more natural language what exactly went wrong.
* We provide extensive documentation to assist you, including both examples and
  Haddocks. Never step on any QuickCheck rakes again!

Currently, we can test the following type class laws:

* `PConstantDecl` and `PUnsafeLiftDecl`

We aim to add more as we go. Furthermore, we also provide helpful instances of
QuickCheck type classes (`Arbitrary`, `CoArbitrary` and `Function`) for several
Plutarch-related data types:

* `Data` (from `PlutusCore.Data`)

# How do I use this?

We provide extensive documentation in the public modules for those who are
already comfortable with `tasty-quickcheck` and property testing in general; for
those who are not, or those who prefer to learn from examples, we provide three
executable examples:

* `examples/square-tests`, which demonstrates basic usage of both universal and
  conditional properties;
* `examples/zip-tests`, which demonstrates conditional properties with crashing
  cases; and
* `examples/natural-tests`, which demonstrates law checking.

Users new to the library can read the examples as code, as well as execute the 
tests with `cabal new-test` to see what they do.

We also have [a wiki
article](https://github.com/Liqwid-Labs/plutarch-quickcheck/wiki/Testing-without-tears:-good-practices-and-tips)
describing good practices for use of this library, as well as QuickCheck and
`tasty-quickcheck` in general. We recommend reading this even for experienced
QuickCheck hands, as there are a lot of useful, and less-known, bits of
information there.

To integrate this with your project, use Nix. We work against the `staging`
branch of Plutarch, so you will have to use it also. See the `flake.nix` file
for more details.

# What can I do with this?

`plutarch-quickcheck` is licensed under the Apache 2.0 license (SPDX code
`Apache-2.0`); please see the `LICENSE` file for more details.
