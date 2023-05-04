# Revision history for `liqwid-plutarch-extra` (aka "LPE")

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 3.21.4 --- 2023-05-04

### Added

- `passetClassDataValueOf` and `passetClassDataValueOfT` to look up the amount 
  of a value corresponding to a `PAssetClassData`.

- `phasOneTokenOfAssetClassData` function for checking that a `PValue` contains
  exactly one of a `PAssetClassData`.

## 3.21.3 -- 2023-03-28

- Add `PTryFrom PData` instance for `PAssetClassData`

## 3.21.2 -- 2023-02-15

### Fixed

- Fix `pgetFullyBoundedTimeRange` always returns nothing

## 3.21.1 -- 2023-01-27

### Modified

- `pguardShowC` is deprecated, due to its on-chain impact for size.

## 3.21.0 -- 2023-01-27

### Modified 

- 'Plutarch.Extra.Value': 'phasOnlyOneTokenOfCurrencySymbol' now correctly
  disallows negative tokens when ensuring presence of only one token.

### Added

- `pwithStateThreadMulti` and `withStateThreadMulti` policies allowing
  to mint more than one state thread token

## 3.20.2 -- 2022-12-10

### Modified

- `Plutarch.Orphans`: `FromJSON` and `ToJSON` of `CurrencySymbol` and
  `TokenName` fixed to actually match the JSON representation that CTL uses this
  time.

## 3.20.1 -- 2022-12-08

### Modified

- `Plutarch.Orphans`: `FromJSON` and `ToJSON` of `CurrencySymbol` and
  `TokenName` are now derived, to match the JSON representation that CTL uses.

## 3.20.0 -- 2022-12-03

### Modified

* Bumped Plutarch to 1.3.0.

### Added

* `applyArguments` to `Plutarch.Extra.Script` since it was dropped on Plutus.
* `Plutarch.Numeric.Additiv` is moved from `plutarch-numeric`.

## 3.19.1 -- 2022-12-05

### Added

- `AsBase16Codec`-based `ToJSON`, `FromJSON` instances for `Datum`

### Modified

- Added `toEncoding` implementation for `AsBase16Bytes` & `AsBase16Codec` `ToJSON` instances

## 3.19.0 -- 2022-12-02

### Modified

* Fixed a bug allowing state tokens with non-empty names to be minted or
  burned.

## 3.18.0 -- 2022-11-30

### Modified

Rename types and functions in module `Plutarch.Extra.Time` in order to avoid
confusion.

* `PCurrentTime` -> `PFullyBoundedTimeRange`
* `pcurrentTime` -> `pgetFullyBoundedTimeRange`
* `currentTime` -> `fullyBoundedTimeRangeFromValidRange`
* `passertCurrentTime` -> `passertFullyBoundedTimeRange`
* `pisWithinCurrentTime` -> `pisWithinTimeRange`
* `pisCurrentTimeWithin` -> `pisTimeRangeWithin`
* `pcurrentTimeDuration` -> `ptimeRangeDuration`

## 3.17.0 -- 2022-11-25

### Modified

* `ToData` instance for `FixedDecimal` now also serializes its type-level tag.
* `FromData` and `UnsafeFromData` instances for `FixedDecimal` now inspect the
  serialized type-level tag to ensure it matches safely.
* `PConstantDecl` for `FixedDecimal` and `PUnsafeLiftDecl` for `PFixedDecimal`
  gain `KnownNat` constraints.
* `PConstantRepr (FixedDecimal unit)` is now `Data`, and `pconstantFromRepr` and
  `pconstantToRepr` defer to the `Data` representation.

## 3.16.0 -- 2022-11-21

### Added

* `ToJSON` and `FromJSON` orphan instances for `PubKeyHash`.

## 3.15.4 -- 2022-11-21

### Added

* `pmax`, `pmin` operating on two terms with a `POrd` instance
* `pmaxBy`, `pminBy`, taking a `PComparator` directly

## 3.15.4 -- 2022-11-17

### Added

* Derived Aeson `ToJSON` & `FromJSON` instances for `PubKeyHash`, `Credential`, `StakingCredential`
* Derived Aeson `ToJSON` & `FromJSON` instances for `FixedDecimal`

## 3.15.3 -- 2022-11-17

### Added

* `eqClasses`, a Haskell equivalent to `peqClasses`.
* `unsafeToAssetClass`, a Haskell equivalent to `punsafeToAssetClass`.

## 3.15.2 -- 2022-11-15

### Added

* `HasLabelled`, a generalization of `HasLabelledGetters` allowing for any type
  of optic.
* `guarantee` and `guarantees`, which parallel `preview` and `previews`, but
  with a default if they 'miss'.

### Modified

* `HasLabelledGetters` is now a synonym for `HasLabelled A_Getter`.
* `HasLabelledGetters` is marked `DEPRECATED`, and will be removed in the next
  major release.

## 3.15.1 -- 2022-11-14

### Added

* `extendedAdaClass` and `pextendedAdaClass` constants for convenience.
* `isExtendedAdaClass` to quickly determine whether we have the ADA extended
  asset class, along with its Plutarch equivalent `pisExtendedAdaClass`.

## 3.15.0 -- 2022-11-11

### Added

* Explain why `punsafeToAssetClassData` and `punsafeToAssetClass` are dangerous
  conversions in general.

### Changed

* `ptoAssetClass` and `ptoAssetClassData` renamed to `punsafeToAssetClass` and
  `punsafeToAssetClassData` respectively.

## 3.14.6 -- 2022-11-11

### Added

* `pextendedAssetClassValueOf'`, a parallel to `passetClassValueOf'`.

## 3.14.5 -- 2022-11-10

### Added

* `ptoAssetClass` and `ptoAssetClassData` for converting `PExtendedAssetClass`
  to `PAssetClass` and `PAssetClassData` respectively.
* `PlyArg` instance for `ExtendedAssetClass`.
* `PlyArgOf` type instance for `ExtendedAssetClass`.

## 3.14.4 -- 2022-11-09

### Added

* `Real` instance for `FixedDecimal`.

## 3.14.3 -- 2022-11-09

### Added

* `inspect`, as a convenient combination of `asks` and `view`.
* `inspects`, as a parallel to `views`.

## 3.14.2 -- 2022-11-09

### Added

* `AssetClass` now has an `UnsafeFromData` instance, derived the same way as
  `FromData` and `ToData`.
* Module `Plutarch.Extra.ExtendedAssetClass`:
  * `ExtendedAssetClass`, designed to provide a runtime distinction between
    `AssetClass`es whose `TokenNames` are arbitrary versus non-arbitrary.
  * Plutarch equivalents to the above: `PExtendedAssetClass`.
  * Helper functions for comparing and retrieving values from
    `PExtendedAssetClass`.

## 3.14.1 -- 2022-11-02

### Added

* In `Plutarch.Extra.Value`:
  - `psymbolValueOf'` for extracting positive and negative amount of a currency
    symbol separately. Particularly useful in a minting policy that supports both
    minting and burning.
* In `Plutarch.Extra.Applicative`:
  - Plutarch level monoid on applicative functors `PAlternative`
  - `PAlternative` instances for:
    * `PMaybe`
    * `PMaybeData`
    * `PList`
    * `PBuiltinList`
  - `ppureIf`
* In `Plutarch.Extra.List`:
  - Delete a value from a list:
    * `pdeleteFirstBy`
    * `ptryDeleteFirstBy`
    * `pdeleteFirst`
  - `plistEqualsBy`
  - Deal with singleton lists:
    * `pisSingleton`
    * `pfromSingleton`
    * `ptryFromSingleton`
* In `Plutarch.Extra.Ord`:
  - `pinsertUniqueBy`
* In `Plutarch.Extra.ScriptContext`:
  - Generate token names:
    * `validatorHashToTokenName` and `pvalidatorHashToTokenName`
    * `scriptHashToTokenName` and `pscriptHashToTokenName`
  - `ptryFromRedeemer` to resolve redeemer
* In `Plutarch.Extra.Time`:
  - `pcurrentTimeDuration`
* In `Plutarch.Extra.Bool`:
  - `passert`

### Modified

* Modified the signature of `pmapMaybe'` in `Plutarch.Extra.List` to allow
  choosing the output list type
* In `Plutarch.Extra.Value`, make `unit` type parameters in the following tagged
  assetclass utilities poly-kinded:
  - `passetClassDataValueT`
  - `psingleValueT'`
  - `passetClassValueOfT'`
  - `passetClassValueOfT`

## 3.14.0 -- 2022-11-01

### Added

* Module `Plutarch.Extra.Deriving` to house derivation helpers.
* Derivation helper for `Semigroup` and `Monoid` via `PInner`, along with a
  _very_ prominent warning about potential misuse.

### Removed

* Overlapping instances of `Semigroup` and `Monoid` via `PInner`.

## 3.13.0 -- 2022-10-31

### Modified

* `pfromInlineDatum` has been renamed `ptryFromInlineDatum`, to match
  conventions.
* `ptryFromOutputDatum` has been renamed `pfromOutputDatum`, to match
  conventions.
* `pfromOutputDatum` has been renamed `ptryFromOutputDatum`, to match
  conventions.
* `pownInput` has been renamed `ptryOwnInput`, to match conventions.
* `pfromDatumHash` has been renamed `ptryFromDatumHash`, to match conventions.
* `pownValue` has been renamed `ptryOwnValue`, to match conventions.

## 3.12.2 -- 2022-10-27

### Added

* Integer power (`#^`) in `Plutarch.Extra.Numeric`.
* `PRationalNoReduce` wrapper in `Plutarch.Extra.Rational`, along with
  conversion functions `pnoReduce` and `preduce'`.

## 3.12.1 -- 2022-10-27

### Added

* In `Plutarch.Extra.FixedDecimal`, zero-cost conversions to/from integers:
  - `toFixedZero`
  - `fromFixedZero`
  - `ptoFixedZero`
  - `pfromFixedZero`

## 3.12.0 -- 2022-10-27

### Added

* `FixedDecimal` Haskell equivalent to `PFixedDecimal`, along with `Num` and
* `Fractional` instances and the following functions:
  - `fixedNumerator`
  - `fixedDenominator`
  - `emul`
  - `ediv`
  - `convertExp`

### Modified

* `PFixedDecimal` is updated so that it represents decimal point range in exponential form.

  Following type and functions are added along with Plutarch numerical instances.
  - `PFixedDecimal`
  - `pfixedNumerator`
  - `pfixedDenominator`
  - `pemul`
  - `pediv`
  - `pconvertExp`
  - `pfromFixedDecimal`
  - `ptoFixedDecimal`
  - `ptoRational`
  - `punsafeMkFixedDecimal`

* old `PFixedDecimal` is renamed and relocated into `Plutarch.Extra.Fixed`.

## 3.11.1 -- 2022-10-27

### Added

* Modifier `GenAssetClass` to provide QuickCheck support for `AssetClass`
* Helper type `AdaClassPresence` for indicating whether `GenAssetClass` should
  generate the ADA class or not

### Modified

* `plutarch-quickcheck` is now a direct dependency, rather than test only.

## 3.11.0 -- 2022-10-25

### Added

* Added a `withStateThread` function (replacing the old function)
  that wraps a minting policy with a unique spend state thread policy

### Modified

* Renamed the old `withStateThread` function to `pwithStateThread`, to
  reflect the fact that it was applied at the plutarch level.

## 3.10.4 -- 2022-10-25

### Changed

* `Plutarch.Extra.AssetClass`:
  * Remove unnecessary `PAsData` wrappers
  * Allow tags of `AssetClass` to be poly-kinded
  * `PlyArg` instance for `AssetClass`

## 3.10.3 -- 2022-10-24

### Added

* Module `Plutarch.Extra.Optics`, containing a utility type family for working
  with labelled-optics-driven records.

## 3.10.2 -- 2022-10-12

### Modified

* `symbolT` and `nameT` are replaced in favor of actual optics.
* `pconstantClass`, `isAdaClass`, `psingleValue'`, `passetClassValueOf'` can now take both
  `Tagged unit AssetClass` and `AssetClass`.

## 3.10.1 -- 2022-10-11

### Added

* Optics for Tagged AssetClasses: `symbolT` and `nameT`. Label optic instance did not work, so
  these helpers were provided.

## 3.10.0 -- 2022-10-06

### Modified

* `Plutarch.Extra.AssetClass`: renamed `pcoerceCls` and `pconstantClass`
  to `pcoerceClass` and `pconstantClass`, respectively.

* `AssetClass`, `PAssetClass`, and `PAssetClassData` now don't have unit tag. Tags should be
  provided with `Tagged` and `PTagged`.

* Tag type tags have been removed from all `AssetClass` utilities.
  - `pisTokenSpent`
  - `passetClassDataValue`
  - `psingleValue`
  - `psingleValue'`
  - `passetClassValueOf`
  - `passetClassValueOf'`
  - `pbyClassComparator'`
  - `phasOneTokenOfAssetClass`
  - `pmatchOrTryRec`

## Added

* Utilities for Tagged Assetclasses
  - `pconstantClsT`
  - `passetClassDataValueT`
  - `psingleValueT'`
  - `passetClassValueOfT`
  - `passetClassValueOfT'`
  - `psymbolAssetClassT`
  - `passetClass`
  - `passetClassData`
  - `passetClassT`
  - `passetClassDataT`

## 3.9.3 -- 2022-10-06

### Added

* `Plutarch.Extra.ScriptContext`, `pfindOwnInput` with V2 types


## 3.9.2 -- 2022-10-04

### Added

* `Plutarch.Extra.StateThread`, with a state thread implementation.

## 3.9.1 -- 2022-09-29

### Added

* `Plutarch.Extra.Bool` : `pcond` function for lisp-like boolean
  conditional-chaining.
* `Plutarch.Extra.Value` :
  * `phasOneTokenOfAssetClass` function for checking
  that a `PValue` contains exactly one of a `PAssetClass`.
  * `phasOneTokenOfSymbol` function for checking that a `PValue` contains exactly
  one token of an `AssetClass`.



## 3.9.0 -- 2022-09-23

### Added

* `Plutarch.Extra.ExchangeRate`: Utilities for working with ExchangeRates at
  the type level.
* `Plutarch.Extra.Rational`: arithmetic and lifting functions for rational types.
* `ToData` and `FromData` instances for `Ratio Integer` to `Plutarch.Orphans`
* `Plutarch.Extra.Value`:
  * `passetClassDataValue` for constructing singleton `PValue`s based on a
    `PAssetClassData`.
  * `pvalue` and `pvaluePositive` for generating a `PValue` from its underlying
    representation, with `NoGuarantees` and `Positive` guarantees, respectively.
  * `passetClassValueOf`, for finding the quantity of a particular `PAssetClass`
    in a `PValue`. A 'ticked' version for working with a Haskell-level
    `AssetClass` also added.
  * `pmatchValueAssets`, for 'pattern-matching' on (underlying representations
    of) `PValue`s.
  * `psplitValue`, for 'separating' the first entry of a `PValue`.
  * A range of `PComparator`s for comparing `PValue`s.
* `Plutarch.Extra.List`:
  * `pfromList`, to turn a Haskell-level list of terms into a `PListLike`.
  * `ptryElimSingle`, which either eliminates a singleton list-like or errors if
    given a non-singleton.
  * `phandleList`, a version of `pelimList` with the arguments re-ordered.
* `Plutarch.Extra.Map`:
  * `plookupGe`, which returns a submap with keys greater than the needle if the
    search is successful.
  * `phandleMin`, a version of `phandleList` for sorted `PMap`s.
* `Plutarch.Extra.Ord`:
  * `plessThanBy` and `pgreaterThanBy`, which are strict comparison versions of
    `pleqBy` and `pgeqBy`.
  * `plessThanMapBy` (and equivalents for the other comparisons), designed for
    comparing two `PMap`s based on a `PComparator` on values, used on shared
    keys.
  * `plessThanValueBy` (and equivalents for the other comparisons), designed for
    comparing two `PValue`s based on a `PComparator` for `PInteger`s, used on
    shared keys.

### Modified

* `Plutarch.Extra.AssetClass`: Modified the old asset-class module to used tagged
  variants. Provides Data and Scott-encoded versions of Assetclasses
  (currency symbol and token name pairs), along with helper functions and
  conversion functions.
  * Field names have changed from `currencySymbol` and `tokenName` to `symbol`
    and `name`, and labeled optics have been added.
* `Plutarch.Extra.ScriptContext`: `pisTokenSpent` now tags a `tag :: Symbol` in
  its type, reflecting the move towards tag AssetClasses
* `Plutarch.Extra.Value`:
  * Changed `passetClassValue` to become `passetClassDataValue`, since it uses
  the data-encoded version.
  * The removal of `psingletonValue` changes the type signature to return a
  `'Sorted`, `'Nonzero` `Value`.
  * The following functions now used tagged `AssetClass`es:
    * `pgeqByClass'`
    * `passetClassValueOf'`
  * `mkSingleValue` renamed to `psingleValue`; its 'ticked' variant is renamed
    analogously.
  * Type arguments for `pvalueOf` are now: `KeyGuarantees`, `AmountGuarantees`,
    `S`, in that order.
  * Type arguments for `padaOf` are now: `KeyGuarantees`, `AmountGuarantees`,
    `S`, in that order.
* `Plutarch.Extra.FixedDecimal`:
  * The removal of `psingletonValue` changes the type signature to return a
  `'Sorted`, `'Nonzero` `Value`.

### Removed

* `Plutarch.Extra.Value`:
 * `psingletonValue`, as this is provided upstream by Plutarch in
   `Plutarch.Api.V1.Value`. This changes type signatures to include a `'Sorted`,
     `NonZero` value (see "Modified" above).
 * `pvalueOf`, as this is provided upstream by Plutarch in
   `Plutarch.Api.V1.Value`.

## 3.8.0 -- 2022-09-26

### Added

* `Plutarch.Extra.Compile`:
  * `mustCompileTracing`, a tracing-enabled equivalent to `mustCompile`.
* `Plutarch.Extra.DebuggableScript`:
  * `DebuggableScript` has label optics for its previous field selectors, under
    the same names.
  * `applyScript`, moved from `Plutarch.Extra.Precompile`.
  * `applyDebuggableArg`, a function for applying an argument to a
    `DebuggableScript` when said argument is provided as a `DebuggableScript.
* `Plutarch.Extra.MultiSig`:
  * `MultiSig` has label optics for its previous field selectors, under the same
    names.
  * `mkMultiSig` for constructing `MultiSig`, which ensures that the structure
    is consistent.
  * `HasField` instance for `"signatories"` in `PMultiSig`.

### Modified

* `Plutarch.Extra.IsData`:
  * `unProductIsData` is now a regular function, not a field accessor.
* `Plutarch.Extra.DebuggableScript`:
  * `DebuggableScript` no longer exports its constructor.
  * `DebuggableScript` no longer has field selectors.
* `Plutarch.Extra.MultiSig`:
  * `MultiSig` no longer exports its constructor.
  * `MultiSig` no longer has field selectors.
  * `PMultiSig` no longer has field selectors.
* `Plutarch.Extra.Precompile`:
  * `debuggableScript` is now a regular function, not a field accessor.
* `Plutarch.Extra.Record`:
  * `runRecordMorphism` is now a regular function, not a field accessor.

### Removed

* `Plutarch.Extra.Precompile`:
  * `applyScript`, now in `Plutarch.Extra.DebuggableScript`.
  * `CompiledTerm'`, as it wasn't being used anywhere or by anything.

## 3.7.1 -- 2022-09-22

### Added

* `applyDebuggableScript` applys `Data` arguments to `DebuggableScript`.

## 3.7.0 -- 2022-09-20

### Moved

* `Plutarch.Extra.Maybe` is entirely moved to
  `Plutonomicon/plutarch-plutus/plutarch-extra`. The module name in
  `plutarch-extra` is identical, so existing code will build without
  problems.

## 3.6.1 -- 2022-09-20

### Added

* `Aeson` instances for Plutus types (in the `Plutarch.Orphans` module)

## 3.6.0 -- 2022-09-14

All additions, removals and changes refer to `Plutarch.Extra.List` unless stated
otherwise.

### Added

* `plookupAssoc`, designed as a very general treatment of list-like structures
  as assoc lists.
* `Plutarch.Extra.Ord` module, containing a new type `POrdering` corresponding
  to the Haskell `Ordering`, as well as `PComparator`, representing a
  \'materialized ordering\'.
* `psort` and `psortBy` in `Plutarch.Extra.Ord`, using sorting networks for
  extra speed, and `PComparator`s.
* `ptryMerge` and `ptryMergeBy` in `Plutarch.Extra.Ord`, for merging list-likes
  sorted by a given `PComparator`.
* `pallUnique` and `pallUniqueBy` in `Plutarch.Extra.Ord`, for verifying the
  uniqueness of the composition of a list-like.
* `pnubSort` and `pnubSortBy`, for eliminating duplicates as well as sorting.
* `precListLookahead` to `Plutarch.Extra.List`, for recursive list elimination
  with a 'look-ahead'.
* `ptryResolveOutputDatum`, an 'erroring' counterpart for `presolveOutputDatum`,
  in `Plutarch.Extra.ScriptContext`.

### Removed

* `pmergeBy` as it is fragile.
* `pmsort` and `pmsortBy`, due to efficiency concerns.
* `pnubSortBy`, `pnubSort`, `pisUniqBy`, `pisUniq`, `pisUniq'`, `pisUniqBy'`
  due to removal of sorting functions or fragility.
* `plookup` and `plookupTuple`, replaced by the more general `plookupAssoc`.
* `pisSorted`, as it is provided by `plutarch-extra` as `pcheckSorted`.
* `pnotNull` and `pfind'`, as they are a bit redundant.
* `pfindDatum` and `ptryFindDatum`, as they're superceded by
  `presolveOutputDatum` and `ptryResolveOutputDatum`, in
  `Plutarch.Extra.ScriptContext`.

### Changed

* `pcheckSorted` and `preverse` are no longer re-exported.
* `pfind'` type arguments are now: content type, list-like
  structure type, `s` tag.
* `pfirstJust` renamed `pfindJust`.
* `pmapMaybe` type arguments are now: list-like structure
  type, 'target' element type, 'source' element type, `s` tag.
* `pfindJust` type arguments are now: 'target' element type, list-like
  structure type, 'source' element type, `s` tag.
* `preplicate` now specifies the order of its type arguments.
* `pisSortedBy` now moved to `Plutarch.Extra.Ord` and uses the new `PComparator`
  functionality.

## 3.5.0 -- 2022-08-30

### Added

* `PlutusTypeDataList` is a new deriving strategy for `ProductIsData`. It will
  only accept Plutarch types that have `PDataRecord` as an `Inner` type.
  Otherwise, it behaves identically to `PlutusTypeNewtype`.

## 3.4.0 -- 2022-08-25

All additions, removals and changes refer to `Plutarch.Extra.Map` unless stated
otherwise.

### Added

* `pmapFromFoldableUnsorted` for constructing unsorted `PMap`s from any
  `Foldable` full of key-value pairs.
* `pmapFromFoldableSorted` for constructing sorted `PMap`s from any `Foldable`
  full of key-value pairs.
* `pkvPairValue` as a value counterpart for `pkvPairKey`.
* `pkeysEqualUnsorted` for doing key set comparisons between unordered `PMap`s.
* `padjust` for applying a function to modify values at a particular key.

### Removed

* `plookup` and `pmap`, as they are now provided by Plutarch itself.
* `pmapFromList`, as it is leaky and unsafe.
* `Plutarch.Extra.Map.Sorted` module, as one of its functions got moved, and the
  other is already provided upstream by Plutarch.
* `Plutarch.Extra.Map.Unsorted` module, as its functionality is either
  suspicious or outright broken throughout.
* `Plutarch.Extra.Value.Unsorted` module, as its functionality is incorrect.

### Changed

* `plookup'` renamed to `ptryLookup`, uses `plookup` internally.
* `pkeys` now produces any `PListLike`, and more clearly specifies its
  guarantees.
* `pupdate`, `pfoldlWithKey` and `pfoldMapWithKey` now only work with
  `Sorted` `PMap`s.
* `pkvPairLt` now only needs a `PPartialOrd` constraint.
* `pkeysEqual` now requires `PIsData k` and `PEq k`, but avoids two intermediate
  lists.
* `pkeysEqual` now only works for sorted `PMap`s.
* `pkeysEqual` now exported from `Plutarch.Extra.Map`.

## 3.3.0 -- 2022-08-23

### Added

* A `Plutarch.Extra.Time` module, containing utilities for working with time and
   time ranges.
* Some utilities for working with closed bounded time ranges, including

  - `PCurrentTime`
  - `pcurrentTime`
  - `currentTime`
  - `passertCurrentTime`
  - `pisWithinCurrentTime`
  - `pisCurrentTimeWithin`

## 3.2.0 -- 2022-08-22

### Changed

* Checks `MultiSig` without the need of passing the whole `PTxInfo`.

## 3.1.0 -- 2022-08-17

### Added

* `Plut` as a replacement for `Top`. This is specialized for kind `S -> Type`.

### Removed

* Uses of `generics-sop` in every module except `Plutarch.Extra.IsData`.

### Changed

* `since`s added to `Plutarch.Extra.Record` functions.
* `(.=)` no longer requires an `SListI` constraint.
* `DeriveGeneric`, `DeriveAnyClass` and `TypeFamilies` are on by default.

## 3.0.3 -- 2022-08-16

### Added

- `#.*`, `#.**`, `#.***` for plutarch function composition. They have similar
  semantics as their counter parts in `Control.Composition`.
- `pfstTuple` and `psndTuple` for `PTuple`.
- Some orphan instances, including

  - `PIsData (PAsData a)`
  - `PTryFrom PData (PAsData PDatumHash)`
  - `PTryFrom PData (PAsData ScriptHash)`
  - `PTryFrom PData (PAsData PUnit)`

- Some useful functions to work with `POutputDatum`.

### Removed

- `pfindTxOutDatum`, please use `presolveOutputDatum` instead.

## 3.0.2 -- 2022-08-09

### Added
 - A `Plutarch.Extra.DebuggableScript` module, containing utilities for lazy
   compilation of scripts-with-tracing as a fallback when the script-without-tracing
   fails. This is useful for testing and benchmarking, since tracing is only turned on
   when error messages are actually needed.
 - A `Plutarch.Extra.Precompile` module, containing utilities for compiling
   scripts and arguments separately and applying them in various ways and from
   various types. This is useful for benchmarking and testing, since it will lead to
   performance increases and more accurate measurements of performance.

## 3.0.1 -- 2022-08-15

### Added

- `PBind` type class, for effect types with meaningful bind semantics. This is a
  direct equivalent to `Bind` from `semigroupoids`.
- `pjoin` and `#>>=`, as direct equivalents to `join` and `>>-` from
  `semigroupoids`, over `Term`s.
- Instances of `PBind` for `PMaybe`, `PMaybeData`, `PList`, `PBuiltinList`,
  `PPair s` (for semigroupal `s`), `PEither e`, `PIdentity` and `PState s`.
- Newtype `PStar` representing Kleisli arrows, as well as some helper functions.
- Instances of `PProfunctor`, `PSemigroupoid`, `PCategory`, `PFunctor`,
  `PApply`, `PApplicative`, `PBind` for `PStar` (in various parameterizations).

## 3.0.0 -- 2022-08-10

This major version bump includes updates to use plutus V2 (post-Vasil) API types.
We have decided that we will _not_ provide backports or updates for V1 API types
in the future.

Where re-exports from `Plutarch.Api.V1` exist, import from the `Plutarch.Api.V2`
modules have be made instead. This will not have any effect on client code, but
should clarify that these functions are indeed suitable for inclusion in V2 scripts.

### Modified
 - Nix flake points at a more recent version of nixpkgs, and temporarily points at a branch of `plutarch-quickcheck`
 - Names of modules referencing specific versions of the API (such as `Plutarch.Api.V1.AssetClass`) have been
   renamed to remove these references (i.e., becoming `Plutarch.Extra.AssetClass`). We will only support the
   more current API version in the future.
 - `pfindTxOutDatum` has been updated to work with V2 style datums (i.e., including a case for inline datums.)

### Removed
 - `plutarch-quickcheck` (aka PQ), which is a dependency of LPE, upgraded to V2 API types as part of a PR that also
   made major changes to its internals. See [here](https://github.com/Liqwid-Labs/plutarch-quickcheck/pull/26).
   As a result, some existing tests for LPE have been temporarily removed. [Issue #53](https://github.com/Liqwid-Labs/liqwid-plutarch-extra/issues/53)
   has been opened to port these tests to PQ2.0

## 2.0.2 -- 2022-08-08

### Changed

 - Scripts compiled with 'mustCompile' now enable deterministic tracing.

## 2.0.1 -- 2022-08-11

### Added

- `pjust` and `pnothing` for easier construction of `PJust` value.
- `pmaybe` which has the same semantics as `Data.Maybe.maybe`.

### Changed

- Rename the original `pamybe` to `pfromMaybe`.

## 2.0.0 -- 2022-08-02

### Added
 - A `Plutarch.Oprhans` module, holding downcasted instances of semigroup and monoid when the upcasted type has the appropriate instances.
 - `pflip` to `Plutarch.Extra.Function`
 - `Plutarch.Extra.IsData` a `PlutusTypeEnumData` as a deriving strategy for `PlutusType`
 - A `Plutarch.Extra.Compile` module, holding a `mustCompile` function to mimic the previous behavior of `compile`

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
