# Revision history for `plutarch-quickcheck`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 2.1.4 -- 2022-10-21

### Added

* Newtype wrapper `TimeDelta` for generating bounded changes in `POSIXTime`.
* Function `withTimeDelta` for CPS-style consumption of `TimeDelta`s.
* Function `timeDeltaProperty` for CPS-style consumption of `TimeDelta`s in a
  `Property` context.

## 2.1.3 -- 2022-10-20

### Added

* `Plutarch.Test.QuickCheck.Instances` module, containing orphan instances for
  QuickCheck type classes for many Plutus types.
* `Plutarch.Test.QuickCheck.Modifiers` module, containing newtype wrappers to
  aid QuickCheck use with types that can be generated in multiple ways:
  * `CurrencySymbol`, with an option to include or exclude the ADA symbol
  * `Value`, with the ability to control ADA and non-ADA entry amounts
* Tests to ensure generators are behaving themselves. Currently only has tests
  for `Value`.

## 2.1.2 -- 2022-09-27

### Modified

* `unTestableTerm` is now a regular function, not a field selector.

## 2.1.1 -- 2022-09-20

### Removed

* Vendored `Plutarch.Extra.Maybe` module is removed as now it is provided by
  `Plutonomicon/plutarch-plutus/plutarch-extra`.

## 2.1.0 -- 2022-09-01

### Added

* `pwrapLam` wraps any Plutarch functions into `PFun` for function generation.
* `punlam'` brings Plutarch level functions into Haskell level functions where
  each term is wrapped in `TestableTerm`. It will require type of the final term.
* `punlam` is identical to `punlam'` but it will evaluate the given Plutarch function
  for better performance.
* `Eqaulity` and `Partiality` option is provided with `haskEquiv` function.

### Changed

* In `Arbitrary` instance of `PArbitrary`, everytime it shrinks, it also gets evaluated.
  This solves the issue with terms getting extremely large when there are multiple shrinks.
* `fromPFun` is now implmented with `pwrapLam` and `punlam`.

## 2.0.0

### Added

* `TestableTerm` hides existential type variable `S` and thus allow generation of
  Plutarch values.
* `PArbitrary` typeclass wraps QuickCheck `Arbitrary`. It defines a generator and shrinker
  of `TestableTerm a`
* `PCoArbitrary` typeclass wraps QuickCheck `CoArbitrary`.
* `fromPFun` bring Plutarch function into Haskell level with `TestableTerm`.
* `haskEquiv` defines properties with a given Haskell equivlance of a Plutarch function.
* `haskEquiv'` is like `haskEquiv` but uses default generators.
* `shrinkPLift` is a shrinker for Plutarch types that have `PLift` instances.
* `arbitraryPLift` is a generator for Plutarch types that have `PLift` instances.
* `PArbitrary` instances for various types.
* `PCoArbitrary` instances for various types.
* Utilities for `TestableTerm`.

## 1.0.2 -- 2022-06-01

### Added

* `classifiedPropertyNative` -- `classifiedProperty` with Haskell functions.

## 1.0.1 -- 2022-05-17

### Added

* `alwaysFailProperty` -- an universial property that ensures script always fails

### Fixed

* Bug that made `classifiedProperty` fail to differentiate between
  wrong result and expected crash when script ran without
  crashing. (Fixed #5)

## 1.0.0 -- 2022-05-05

### Added

* First release
