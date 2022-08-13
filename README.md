# `plutarch-quickcheck`
Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-libs

## What is this?

A helper library to write Plutarch-oriented QuickCheck property tests. 
Interfaces `plutarch-quickcheck` provide are designed to look and feel
like writting regular `QuickCheck` properties. 

## What _exactly_ does this do for me?

`plutarch-quickcheck` *lifts* a property written in Plutarch code into 
`QuickCheck`. For example,

```hs
additionCommutative :: Term s (PInteger :--> PInteger :--> PBool)
additionCommutative = plam $ \x y -> x + y #== y + x

quickCheck $ fromPFun additionCommutative -- Magic!
-- +++ OK, passed 100 tests.
```

The library also provides other utilities such as generation of 
arbitrary Plutarch functions and defining properties for Plutarch functions 
verified by the equivalent Haskell definition. Currently, only functions with 
a single arguement are supported.

```hs
pfunctionGeneration :: Term s (PFun PInteger PInteger :--> PBool)
pfunctionGeneration = plam $ \(PFn (f :: Term s (PInteger :--> PInteger))) -> ...

reverseProperty :: Property
reverseProperty = haskEquiv' reverse (preverse @PBuiltinList)
```

# How do I use this?

We provide extensive documentation in the public modules for those who are
already comfortable with `tasty-quickcheck` and property testing in general; for
those who are not, or those who prefer to learn from examples, we provide three
executable examples:

* `examples/reverse-tests`, which demonstrates how to define a property 
using a Haskell definition for intended behavior.
* `examples/zip-tests`, which demonstrates how to use `fromPFun` to 
define a Plutarch property. 

Users new to the library can read the examples as code, as well as execute the
tests with `cabal test` to see what they do.

We also have [a wiki
article](https://github.com/Liqwid-Labs/plutarch-quickcheck/wiki/Testing-without-tears:-good-practices-and-tips)
describing good practices for use of this library, as well as QuickCheck and
`tasty-quickcheck` in general. We recommend reading this even for experienced
QuickCheck hands, as there are a lot of useful, and less-known, bits of
information there.

To integrate this with your project, use Nix. We work against the `staging`
branch of [Liqwid Labs' fork of
Plutarch](https://github.com/Liqwid-Labs/plutarch), so you will have to use it
also. See the `flake.nix` file for more details.

# What can I do with this?

`plutarch-quickcheck` is licensed under the Apache 2.0 license (SPDX code
`Apache-2.0`); please see the `LICENSE` file for more details.
