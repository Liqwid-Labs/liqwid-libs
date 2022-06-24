# Revision history for `liqwid-plutarch-extra`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 1.1.0 -- 2022-06-24

### Added

- Convenience wrapper for `DerivePNewtype`: `DerivePNewtype'`, `DerivePConstantViaNewtype'`
- Encode product types as lists: `ProductIsData`, `DerivePConstantViaDataList`
- Encode enum types as integers: `EnumIsData`, `PEnumData` and `DerivePConstantViaEnum`
- Plutarch helper functions: `pmatchEnum`, `pmatchEnumFromData`

### Changed

- Rename `PConstantViaDataList` to `DerivePConstantViaDataList`

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
