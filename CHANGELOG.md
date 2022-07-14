# Revision history for `liqwid-plutarch-extra`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

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

## 1.0.0 -- 2022-05-24

### Added

* First release
