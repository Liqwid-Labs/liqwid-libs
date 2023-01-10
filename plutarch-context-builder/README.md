# `plutarch-context-builder`
Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-plutarch-extra.
Issues and project-management-related information are tracked on [Notion](https://www.notion.so/liqwid/).

## What is this?

A library containing builders for `ScriptContext`s. Currently, we support
building `ScriptContext`s with the `Spending` and `Minting` `ScriptPurpose`s.

## What _exactly_ does this do for me?

Building a `ScriptContext` manually is sometimes necessary: testing is a good
example. However, doing this directly is tedious, error-prone and extremely
unclear in many places; this leads to significant wasted time and confusion. 

`plutarch-context-builder` aims to make this as easy as possible. We do this
through a combination of higher-level operations to describe what the script
should do, along with a `Monoid`-based API to put things together. Users of
this library don't have to deal with the particulars of how
`ScriptContext`s are defined if they don't want to, but they can still generate
them reliably and with minimal pain. More, the monoidal API allows overrides on
specific fields; for example, overriding Transaction ID. This allows users to
create a _slightly_ different script context without laborious record 
overriding or reconstructing contexts from scratch.


`plutarch-context-builder` fits into the testing strategy of Liqwid Labs by 
providing an ergonomic interface for "sample tests" in conjunction with 
[`plutarch-unit`](https://github.com/Liqwid-Labs/plutarch-unit)(a `tasty` provider).
Sample tests are useful for

- Demonstrating the intended "happy path" of a script or transaction family
- Unit testing individual validators in isolation (i.e., _not_ testing a full transaction).
  This can be useful, for instance, if multiple related validators are required to 
  test a full transaction, but not all of them are fully implemented.
- Unit testing helper functions that work on a subset of the fields of a `ScriptContext` or 
  `TxInfo`, but that don't fit the `Datum -> Redeemer -> ScriptContext -> ()` of a validator
- Regression testing -- making specific, individual test cases when things break.

For other testing libraries that complement the functionality of `plutarch-context-builder`, 
see:

- [plutus-simple-model](https://github.com/mlabs-haskell/plutus-simple-model)
- [plutip](https://github.com/mlabs-haskell/plutip)
- [plutarch-quickcheck](https://github.com/Liqwid-Labs/plutarch-quickcheck)
- [plutarch-unit](https://github.com/Liqwid-Labs/plutarch-unit)
- [plutarch-benchmark](https://github.com/Liqwid-Labs/plutarch-benchmark)

# How do I use this?

Everything is done monoidally in `plutarch-context-builder`, from setting 
`UTXO` to adding values. Users can use `<>` to combine interface 
functions into larger script contexts.

There are two main types to consider: `UTXO` and `Builder a`. The former 
constructs single UTXO in transactions:

```hs
data UTXO = UTXO
    { utxoCredential :: Maybe Credential
    , utxoStakingCredential :: Maybe StakingCredential
    , utxoValue :: Value
    , utxoData :: Maybe DatumType
    , utxoReferenceScript :: Maybe ScriptHash
    , utxoTxId :: Maybe TxId
    , utxoTxIdx :: Maybe Integer
    }
```

There are a few things to note. First, a `UTXO` can 
be shared in different contexts as both input or output; `TxOutRef`-related
fields get ignored when a `UTXO` is being used as an output. Second, besides 
`Value`, the binary operator(`<>`) will _replace_ previous values. It allows users
to reuse pre-defined `UTXO`s with updated information. Note that the binary operator 
will always update the last given data.


```hs
let utxo = address "aabbcc" <> withStakingCredential (StakingPtr 1 2 3)
in utxo <> withStakingCredential (StakingPtr 7 8 9)
-- utxo is "updated" and now has 'StakingPtr 7 8 9'
```

`Builder a` primarily appends instead of replacing. It will collect 
multiple inputs, reference inputs, outputs, signatures, et cetra 
and build the context out of it. User can expect it to combine similarly 
to regular lists. `Builder a` will only replace the `TxId` and time range.

`Builder a` is a typeclass, containing "common" script-building 
interfaces. Using this, `plutarch-context-builder` defines specific 
builders such as `SpendingBuilder`, `MintingBuilder`, and `TxInfoBuilder`.
These specific builders are specialized on handling the `ScriptPurpose` 
when a full `ScriptContext` is generated; they will require calling some 
special interfaces --still Monoidal-- to build the `ScriptContext` properly.

# Example

The example source can be found in the `example` directory.

```hs
-- What can be shared between contexts
commonContext :: (Monoid a, Builder a) => a
commonContext =
	mkOutRefIndices $ -- automatically assigns OutRefIdx
    mconcat
        [ input $
            pubKey "aabb"
                <> withValue (singleton "cc" "hello" 123)
                <> withRefIndex 5
                <> withStakingCredential (StakingPtr 0 0 0)
        , input $
            address (Address (PubKeyCredential $ PubKeyHash "aa") (Just $ StakingPtr 1 2 3))
                <> withValue (singleton "cc" "hello" 123)
                <> withDatum (123 :: Integer)
                <> withRefTxId "eeff"
        , output $
            script "cccc"
                <> withValue (singleton "dd" "world" 123)
        , mint $ singleton "aaaa" "hello" 333
        ]
		
spendingContext :: SpendingBuilder
spendingContext = commonContext <> withSpendingOutRefIdx 5

spendingContext2 :: SpendingBuilder
spendingContext2 = commonContext <> withSpendingOutRefId "eeff"

mintingContext :: Mintingbuilder
mintingContext = generalSample <> withMinting "aaaa"

-- Build contexts
buildMinting mempty mintingContext
buildSpending mempty spendingContext

-- Builders that will return a product type instead of throwing error.
tryBuildMinting mempty mintingContext
tryBuildSpending mempty spendingContext
```		

## Checkers

`plutarch-context-builder` provides various checkers and some combinators
to construct custom ones. It comes with quasi-phase-1 validation that
will ensure all bytestrings are of the correct length, the inflow and outflow 
of the context is equal, at least one signature is provided, et cetra. 

Users can also construct a custom checker with a common contravariant
interface: 

```hs
data MyError = MoreThanOneInput

ensureOnlyOneInput :: Builder a => Checker MyError a
ensureOnlyOneInput = 
	checkAt AtInput $
	mconcat $
	[ contramap ((== 1) . length . bbInputs . unpack) (checkBool $ OtherError MoreThanOneInput)
	, ... -- Multiple checkers can be combined as well.
	, ...
	]
	
-- Will return `Acc` list of errors
-- Empty list when no error is found
runChecker ensureOnlyOneInput context

-- User can run phase-1 validation with
buildSpending checkPhase1 context

```

If more sophisticated validation is required, `plutus-simple-model` 
should be used. `plutus-simple-model` uses functions directly from `cardano-ledger`,
and thus is more true-to-life than `plutarch-context-builder`.
`plutarch-context-builder` aims to test simpler functionalities very
quickly and intuitively, while `plutus-simple-model` almost simulates
an entire contract flow.

# Standards

The `plutarch-context-builder` code base follows strict standards to increase consistency, to minimize
the impact of legacy, to properly use automated tools, and more. The standards document
can be found [here](https://liqwid.notion.site/Coding-Standards-cd3c430e6e444fa292ecc3c57b7d95eb).
