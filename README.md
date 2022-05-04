# `plutarch-quickcheck`

## What is this?

A helper library to write Plutarch-oriented QuickCheck property tests. It
includes support for both 'general' QuickCheck-based property testing, as well
as testing (some) Plutarch type class laws. These helpers are designed to
integrate seamlessly with
[`tasty-quickcheck`](https://hackage.haskell.org/package/tasty-quickcheck).

## What _exactly_ does this do for me?

QuickCheck is a folkloric minefield, and can be quite tedious to use correctly.
Furthermore, its interactions with Plutarch can be quite non-obvious, and a lot
of the built-in functionality it provides is not very useful for it.
Furthermore, while Plutarch type classes do have laws, verifying them by hand
can be quite difficult and error-prone. Lastly, the error output of QuickCheck
(and by extension, `tasty-quickcheck`) can be quite hard to make sense of.

`plutarch-quickcheck` aims to fix all of these issues:

* As long as you can provide generators and shrinkers, you don't have to
  interact with any other part of QuickCheck _at all_; the boilerplate is done
  for you. This applies equally to both general properties and laws checks.
* Issues of QuickCheck coverage, especially for conditional properties, are
  handled automatically: you never have to consider this.
* Whenever possible, `plutarch-quickcheck` will ensure that things are defined,
  and behaving, sensibly relative one another, and will inform you (quite
  loudly) if not.
* Error output tries to be as helpful as it can, both by using prettyprinting
  and also explaining in more natural language what exactly went wrong.

Currently, we are able to test the following type class laws:

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

These are designed to be read as code, but can also be executed to see what they
do.

To integrate this with your project, use Nix. We work against a [fork of
Plutarch](https://github.com/peter-mlabs/plutarch) (the `liqwid/extra` branch),
so you will have to use it also. See the `flake.nix` file for more details.

# What can I do with this?

TODO when released
