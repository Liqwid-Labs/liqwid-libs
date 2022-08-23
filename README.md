# plutarch-unit
Unit tests for Plutarch

Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-libs

## What is this?

A helper library to write Plutarch-oriented unit tests using `tasty`. 

## What _exactly_ does this do for me?

You can use this library to write `tasty` test trees without a dependency on `hspec`,
`hunit`, or similar. We believe that `tasty` is more extensible and better maintained 
than the alternatives.

This library currently provides interfaces to write positive and negative annotated test 
cases for scripts (in general), minting policies, and validators.

## How do I use this?

This library contains a single module with self-explanatory functions, provided that 
users know how to write `tasty` code (which we assume). A basic example follows below:

```hs
tests:: TestTree
tests =
  testGroup
    "My first tests"
    [ test1
    , test2
    , test3
    ]
  where
    test1 :: TestTree
    test1 =
      mintingPolicySucceedsWith
        "The minting policy succeeded when it should
        myMintingPolicy
        myRedeemr
        myScriptContext
```        

To integrate this with your project, use Nix. We work against the `master`
branch of [Plutarch](https://github.com/Plutonomicon/plutarch-plutus), so you will have to use it
also. See the [`flake.nix`](./flake.nix) file for more details.

# What can I do with this?

`plutarch-unit` is licensed under the Apache 2.0 license (SPDX code
`Apache-2.0`); please see the `LICENSE` file for more details.
