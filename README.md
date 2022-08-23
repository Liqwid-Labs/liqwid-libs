_Note, 2022-06-011_: this repository is alpha-level software. Stability or correctness should not be assumed.

# `plutarch-context-builder`
Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-plutarch-extra

## What is this?

A library containing builders for `ScriptContext`s. Currently, we support
building `ScriptContext`s with the `Spending` and `Minting` `ScriptPurpose`s.

## What _exactly_ does this do for me?

Building a `ScriptContext` manually is sometimes necessary: testing is a
good example case. However, doing this directly is tedious,
error-prone, and extremely unclear in many places; this leads to
significant wasted time and confusion.

plutarch-context-builder aims to make this as easy as possible. We do
this through a combination of higher-level operations to describe what
the script should do, along with a `Monoid`-based API to put things
together. If you use this library, you don't have to deal with the
particulars of how `ScriptContexts` is defined if you don't want to, but
you can still generate them reliably and with minimal pain. More, the
monoidal API allows overrides on specific fields; for example,
overriding Transaction ID. It allows users to create a _slightly_
different script context without laborious record overriding or
reconstructing contexts from scratch.

# How do I use this?

Everything is done monoidally in `plutarch-context-builder` from setting
`UTXO` to adding values. Users can use `<>` to combine each interface
function into larger script contexts. 

There are two main types to consider: `UTXO` and `Builder a`. Former
constructs a single UTXO in transactions.
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

Few things to note. First, `UTXO` represents both input and output. It
allows the same `UTXO` to be shared in different contexts as both input
and output. Only `TxOutRef`-related fields get ignored when it is being
used as an output. Second, besides Value, the binary operator(`<>`) will
replace the previous value. It allows users to reuse pre-defined UTXO
with updated information. Note, that the binary operator will always
update the last given data.

```hs
let utxo = address "aabbcc" <> withStakingCredential (StakingPtr 1 2 3)
in utxo <> withStakingCredential (StakingPtr 7 8 9)
-- utxo is "updated" and how have 'StakingPtr 7 8 9'
```

`Builder a` mostly appends instead of replacing. With will collect
multiple inputs, reference inputs, outputs, signatures, et Cetra and
build the context out of it. Users can expect it to combine similarly
to regular lists. `Builder a` will only replace `TxId` and time range.

`Builder a` is a typeclass, containing "common" script-building
interfaces. Using this, `plutarch-context-builder` defines specific
builders: like `SpendingBuilder`, `MintingBuilder`, and
`TxInfoBuilder`. These specific builders are specialized in handling
`ScriptPurpose`; they will require having some special interfaces--still
Monoidal-- to build context properly.

# Sample

The sample can be found in `/sample`.

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
will ensure all bytestrings have correct length, inflow and outflow 
of context is equal, at least one signature is provided, et Cetra. 

User can also construct a custom checker with a common contravariant
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
should be used because it runs the entire Cardano Ledger. 
`plutarch-context-builder` aims to test simpler functionalities very
quickly and intuitively, while `plutus-simple-model` almost simulates
whole contract flow.
