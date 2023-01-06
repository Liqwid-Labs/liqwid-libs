# Revision history for `plutarch-context-builder`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 2.11.0 -- 2023-01-06

* Added `dcert`

## 2.10.0 -- 2022-11-29

### Modified

* Bump to liqwid-nix 2.0 and Plutarch 1.3.0.

## 2.9.0 -- 2022-11-21

### Modified

* Made `buildTxInInfo` return `TxOutRef` with default value instead of Nothing
  when given no TxId or TxIdx.

## 2.8.0 -- 2022-11-16

Added builders for `Rewarding` and `Certifying` contexts.

### Added

* Added `RewardingBuilder`, `withRewarding`, and `buildRewarding'`.

* Added `CertifyingBuilder`, `withCertifying`, and `buildCertifying'`.

## 2.7.0 -- 2022-11-11

Added "withdrawal" capabilities.

### Added

* Added a `withdrawal :: Builder a => StakingCredential -> Integer -> a` function to
  `Plutarch.Context`.

### Modified

* Added a `withdrawals` field to the base builder, along with updating the relevant
  optics.

## 2.6.2 -- 2022-11-1

### Modified

* `checkMints` fixed

## 2.6.1 -- 2022-10-20

### Added

* `continuing` and `continuingWith`, allowing easy continuing of 'Value'.

## 2.6.0 -- 2022-09-27

### Modified
* `normalizeValue` now has the correct behavior of:
  * adding a 0 Ada entry to the underlying map if the
    entry would otherwise be missing.
  * removing entries with the Ada currency symbol, but a non-Ada
    token name

## 2.5.0 -- 2022-09-26

### Added

* Labelled optics for `BaseBuilder`, `MintingBuilder`, `SpendingBuilder`,
  `UTXO` and `Mint`. These follow the names of the old field selectors, but
  without disambiguation prefixes: for example, `bbMints` is now `#mints`.

### Modified

* `BaseBuilder`, `MintingBuilder`, `SpendingBuilder`, `UTXO` and `Mint` no
  longer export their constructors.
* `runChecker` is now a regular function, not a field selector.
* `checkBSLength` replaced old `checkByteString`. It checks if `ByteString` is
  at given length.

### Removed

* Field selectors for `BaseBuilder`, `MintingBuilder`, `SpendingBuilder`,
  `UTXO` and `Mint`.

### Fixed

* `checkTxId` is fixed to follow the new ledger spec and will now look for 32
  bytes long `ByteString`.

## 2.4.0 -- 2022-09-14

### Added

* `normalizePair`, `normalizeMap`, `normalizeValue`, `normalizeUTXO`,
  and `normalizeMint` added for normalizing the data. It will combine
  elements with the same "key" with given concatenation function.
* `sortMap` sorts `AssocMap.Map` by the key.
* `mkNormalized` added for normalizing all `Value`s and `Mint`s in `Builder`.
* `checkNormalized` checks if values in the builder are normalized.
* `checkValueNormalized` checks if the given value is normalized.

## 2.3.0 -- 2022-09-13

### Added

* Redeemer map support in general.
* `withRedeemer` attaches a redeemer to a UTxO.
* `mintWith` and `mintSingletonWith` allow minting with a given redeemer
* `checkValidatorRedeemer` ensures that given UTxO have redeemer, if owned by any validator.

### Modified

* `mint` sets the redeemer used for minting to `()` by default.

## 2.2.0 -- 2022-08-22

### Added

* `address` is similar to `credential`, `script`, and `pubKey` but takes `Address` instead.
  It will take `StakingCredential` if the address is provided with one.
* `withStakeCredential` is for adding `StakingCredential` to an UTXO.

## 2.1.0 -- 2022-08-09

### Added

* `Checker` is in charge of composing and constructing checks over `ScriptContext`.
  It is composed with the `contravariant` typeclasses.
* `referenceInput` is added for reference inputs in V2 `ScriptContext`. It act similarly
  to `input` and `output`.
* `withInlineDatum` is for new inline-datum feature in V2. It acts similarly to `withDatum`,
  but it doesn't add datum to `ScriptContext`.
* `mkOutRefIndices` modifies given builder to have "correct" indices.
* `check[Spending|Minting]` are checkers for each context builder.
* `build[Spending|Minting]'` are builder that never fails. If given information is insufficient it will
  use dummy values. Thus, it is advisable to `build[Spending|Minting]` which comes with minimum checks
  to ensure not to use dummy values.
* `tryBuild[Spending|Minting]` first run `check[Spending|Minting]` and checkers given in argument.
  Then, it returns either `ScriptContext` or list of errors.
* `CheckerError` is sum of `CheckerPos` and `CheckerErrorType`
* `CheckerPos` is possible positions of the errors.
* `CheckerErrorType` is possible errors from checkers.
* `basicError` constructs most general error from given error type.
* `checkAt` updates/overrides checker position.
* `checkFoldable` applies checker of type `a` to foldable `t a`.
* `checkIf` makes checker from predicate and error. It returns given error if predicate is false.
* `checkIfWith` is a combination of `checkIf` and `CheckWith`
* `checkBool` checker for `Bool`. It returns given error when false.
* `checkWith` builds checker via CPS computation.
* `checkByteString` checks if given `ByteString` is valid.
* `checkPositiveValue` checks if given `Value` only contains Positive value.
* `checkTxId` checks if given `TxId` is valid.
* `handleErrors` runs given `Checker a` with given `a`, returns `a` if checker succeeds, throws error if not.
* `checkSignatures` checks if signatures are correct in given builder.
* `checkZeroSum` checks if builder have equal inflow and outflow of tokens.
* `checkInputs` checks if builder have a valid inputs. It checks credentials, values, and output references.
* `checkReferenceInputs` checks i builder have a valid reference inputs. It checks credentials and values.
* `checkFail` is a checker that always fail with given error.
* `checkMints` checks if builder have a valid mints. It ensures there are no Ada minted.
* `checkFee` checks if builder have a valid fee. It ensures fee only contains Ada value.
* `checkOutputs` checks if builder have a valid outputs. It checks credentials and values.
* `checkDatumPairs` checks if builder have any extra datum-datumhash pair.
* `checkPhase1` is a list of all subcheckers that is required to call a `ScriptContext` phase-1 valid.
* `renderErrors` prettifies given foldable structure of `CheckerError`.
* `flattenValue` flattens given Value into list of tuples.

### Modified

* `Builder` typeclass now requires `Lens' a BaseBuilder` instead of `unpack` to provide setter
  functionalities--used by `mkOutRefIndices`.
  * `unpack` is now a separate function.
* `withTxId` is renamed to `withRefTxId` since it can be confused with `TxId` of `TxInfo`.
* `withOutRef` is renamed to `withRef` in order to follow rest of `withRef*` interfaces.
* `build[Spending|Minting]` are now identical to `tryBuild[Spending|Minting]` but it throws error when
  checker fails.

### Deleted

* `build[Spending|Building|TxInfo]Unsafe` is deprecated in favor of new builders that use checkers.

## 2.0.0 -- 2022-07-25

### Added

* `UTXO` is now properly exposed to the users.
* `Semigroup` and `Monoid` instances for `UTXO`
* `SubBuilder` provides minor builders that produces context smaller than `TxInfo`.
  * `buildTxOut` builds a single `TxOut` from given `UTXO`.
  * `buildTxOuts` builds a list of `TxOut`s from given `SubBuilder`.
  * `buildTxInInfo` builds a single `TxInInfo` from given `UTXO`.
  * `buildTxInInfos` builds a list of `TxInInfo`s from given `SubBuilder`.
  * `buildDatumHashPairs` builds a list of Datum-Hash pair from all
    inputs, outputs, extra data of given builder.
* `TxInfoBuilder` provides builder for builder `TxInfo`; it is a `newtype` wrapper of `BaseBuilder`.
  * `buildTxInfo` builds `TxInfo` from given `TxInfoBuilder`.
* `withSpendingUTXO` sets input validator from given `UTXO`.
* `withSpendingOutRef` sets input validator from given `TxOutRef`.
* `withSpendingOutRefId` sets input validator from given `TxOutRefId`.
* `withSpendingOutRefId` sets input validator from given `TxId`.
* `withSpendingOutRefIdx` sets input validator from given reference index, which is `Integer`.

### Modified

* `UTXO` is now constructed with Monoidal interfaces similar to `BaseBuilder`.
* Multiple `withValue`s will now combine values instead of replacing them.
* `withSpendingUTXO` is updated to use updated `UTXO`.

## 1.0.0 -- 2022-05-18

### Added

* First release
