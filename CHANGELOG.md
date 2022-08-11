# Revision history for `plutarch-quickcheck`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 2.0.0

### Added

* `TestableTerm` hides existential type variable `S` and thus allow generation of
  Plutarch values.
* `PArbitrary` typeclass wraps QuickCheck `Arbitrary`. It defines generator and shrinker
  of `TestableTerm a`
* `PCoArbitrary` typeclass wraps QuickCheck `CoArbitrary`. 
* `fromPFun` bring Plutarch function into Haskell level with `TestableTerm`.
* `haskEquiv` defines properties with given Haskell equivlance of Plutarch function.
* `haskEquiv'` is like `haskEquiv` but use default generators. 
* `shrinkPLift` is shrinker for Plutarch type that is `PLift`.
* `arbitraryPLift` is generator for Plutarch type that is `PLift`.
* `PArbitrary` instances for various types.
* `PCoArbitrary` instances for various types.
* Utilities for `TestableTerm`.

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
