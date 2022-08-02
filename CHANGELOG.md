# Revision history for `liqwid-plutarch-extra`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 1.4.0 -- 2022-08-02

### Added 
 - A `Plutarch.Oprhans` module, holding downcasted instances of semigroup and monoid when the upcasted type has the appropriate instances.
 - `pflip` to `Plutarch.Extra.Function`
 - `Plutarch.Extra.IsData`:
   - `PDerivePDataFieldsViaDataList`
   - A `PlutusTypeEnumData` as a deriving strategy for `PlutusType` 

### Changed

 - Update to [`Liqwid.nix`](https://github.com/liqwid-Labs/liqwid-nix)
 - Update to Plutarch version 1.2. See the [CHANGELOG](https://github.com/Plutonomicon/plutarch-plutus/blob/v1.2.0/CHANGELOG.md) 
   for full details.
   - The flake now points at the `Plutonomicon` repository, instead of the Liqwid Labs fork.
   - Changes to deriving strategies and constraints may cause some API breakage. In particular,
     `deriving via`, `PMatch`, `PCon` has been eliminated, and redundant `PAsDAta`, `pfromData` have been reduced.

### Removed

 - The `Plutarch.Extra.Other` module has been removed. This held `deriving via` wrappers that are no longer necessary.
 - Tests relating to `Value`s and unsorted `Map`s, since `Plutarch 1.2` removed the `PEq` constraint on unsorted maps.

## 1.3.0 -- 2022-07-20

### Added

- `pmatchAll` and `pmatchAllC`, `pletFields` that gets all Plutarch record fields.
- `Plutarch.Extra.MultiSig`, a basic N of M multisignature validation function.
- `pscriptHashFromAddress`, gets script hash from an address.
- `pisScriptAddress`, checks if given address is script address.
- `pisPubKey`, checks if given credential is a pubkey hash.
- `pfindOutputsToAddress`, finds all TxOuts sent to an Address.
- `pfindTxOutDatum`, finds the data corresponding to a TxOut, if there is one.
- `phasOnlyOneTokenOfCurrencySymbol`, checks if entire value only contain one token of given currency symbol.
- `pon`, mirroring `Data.Function.on`.
- `pbuiltinUncurry`, mirroring `uncurry`.
- `pmaybeData`, mirroring `maybe` for `PMaybeData`.
- `pdjust` for easier construction of `PDJust` value.
- `pdnothing` for easier construction `PDNothing` value.

### Modified

- Fixed `PApplicative` instances that previously not worked due to not using `pfix`.
- Renamed `PType` to `S -> Type`.
- Renamed `mustBePJust` to `passertPJust`.
- Renamed `mustBePDJust` to `passertPDJust`.

## 1.2.0 -- 2022-07-12

### Added

- `PBoring` type class, representing singleton types.
- Instances of `PBoring` for various types.
- `preconst` for `PConst`, which allows safe coercions between different
  'pretend' types.
- `PSemiTraversable` instance for `PTagged`.
- `preplicateA` and `preplicateA_`, allowing for repeated execution of
  `PApplicative`.
- `pwhen` and `punless`, mirroring their Haskell counterparts.
- `preplicate`, mirroring its Haskell counterpart.

### Modified

- `PFunctor` now has a `pfconst` method as a back-end for `#$>` and `#<$`. This
  has a default implementation in terms of `pfmap`.
- `pvoid` can now replace every location with any `PBoring`, not just `PUnit`.
- `PTraversable` now has a `ptraverse_` method, which allows us to avoid
  rebuilding the `PTraversable` if we don't need it anymore. This allows much
  better folding, for example.
- `PSemiTraversable` now has a `psemitraverse_` method, with similar benefits to
  `ptraverse_`.
- `psemifold`, `psemifoldMap` and `psemifoldComonad` gained a `PSubcategory t a` 
  constraint, as the 'container' is guaranteed non-empty in such a case.
- Significant performance improvements for `PTraversable` and `PSemiTraversable`
  instances.

## 1.1.0 -- 2022-06-17

### Added

- Convenience wrapper for `DerivePNewtype`: `DerivePNewtype'`, `DerivePConstantViaNewtype'`
- Encode product types as lists: `ProductIsData`, `DerivePConstantViaDataList`
- Encode enum types as integers: `EnumIsData`, `PEnumData` and `DerivePConstantViaEnum`
- Plutarch helper functions: `pmatchEnum`, `pmatchEnumFromData`

#### AssocMap (`Plutarch.Extra.Map`)

- `pupdate`
- `pmapMap` -> `pmap`

#### AssocMap (`Plutarch.Extra.Map.Sorted`)

- `pkeysEqual`
- `pmapUnionWith` -> `punionWith`

#### AssocMap (`Plutarch.Extra.Map.Unsorted`)

- `psort` 
- `pkeysEqual`
- `pmapUnionWith` -> `punionWith`

#### Value (`Plutarch.Api.V1.Value`)

- `psymbolValueOf`
- `passetClassValueOf'`
- `pgeqByClass`
- `pgeqByClass'`
- `pgeqBySymbol`

#### Value (`Plutarch.Api.V1.Value.Unsorted`)

- `psort`

#### Maybe (`Plutarch.Extra.Maybe`)

- `pisJust`
- `pisDJust`
- `pfromMaybe`  -> `pmaybe`
- `tcexpectJust` (in `Plutarch.Extra.Maybe`) -> `pexpectJustC` (in `Plutarch.Extra.TermCont`)
- `pmaybeToMaybeData`

#### List (`Plutarch.Extra.List`)

- Re-exports from `plutarch-extra`
- `pnotNull`
- `pnubSortBy`/`pnubSort`
- `pisUniqueBy`/`pisUnique`
- `pmergeBy`
- `pmsortBy`/`pmsort`
- `pfindMap`->`pfirstJust`
- `plookup`
- `plookupTuple`
- `pfind'`
- `pfindMap` -> `pfirstJust`

#### `TermCont` (`Plutarch.Extra.TermCont`)

- Re-exports from `plutarch-extra` 
- `tcassert` -> `passertC`
- `pguardWithC`
- 'pguardShowC'
- `tcexpectJust` (in `Plutarch.Extra.Maybe`) -> `pexpectJustC` (in `Plutarch.Extra.TermCont`)

#### Script Context (`Plutarch.Api.V1.ScriptContext`)

- `ptokenSpent` -> `pisTokenSpent`
- `pisUTXOSpent`
- `pvalueSpent`
- `ptxSignedBy`
- `ptryFindDatum`
- `pfindDatum`
- `pfindTxInByTxOutRef`

### Modified

- Rename `PConstantViaDataList` to `DerivePConstantViaDataList`

## 1.0.0 -- 2022-05-24

### Added

* First release
