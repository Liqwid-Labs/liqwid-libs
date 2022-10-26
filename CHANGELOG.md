# Revision history for `plutarch-unit`

## 1.3.0 -- ???

## 1.2.0 -- 2022-10-07

### Added

* `Test.Unit.Precompiled`:
  - Generic instances and labeled optics for `TestCase` and `TestCompiled`
  - `tryFromPTerm`: a partial function generating a debuggable test
    case from a `ClosedTerm`
  
### Modified
* `Test.Unit.Precompiled`: `fromPTerm`'s type signature changed to 
  return a `Either Text TestTree` and is now total. For the partial 
  version, see `tryFromPTerm`
   
### Deleted

* `Test.Unit.Precompiled`: `fromScript`, because invariants could
  be broken. 

## 1.1.0 -- 2022-09-21

### Added

* `Test.Unit.Precompiled` module for testing precompiled scripts.
* `TestCompiled` provides monadic interface for precompiled script checking.
* `Expectation` defines if a test case should succeed or not.
* `(@&)` and `withApplied` stitches in shared arguments.
* `testEvalCase`, `@>`, and `@!>` tests if script fails or succeeds given arguments.
* `testEqualityCase` tests if script is eqaul to expectation or not given arguments.
* `fromPTerm` compiles Plutarch term and tests in `TestCompiled`.

## 1.0.0 -- 2022-07-26

### Added

* `validatorSucceedsWith`, ensuring validator runs successfully given datum, redeemer, and script context.
* `validatorFailsWith`, ensuring validator fails given datum, redeemer, and script context.
* `mintingPolicySucceedsWith`, ensuring minting policy runs successfully given datum, redeemer, and script context.
* `mintingPolicyFailsWith`, ensuring minting policy fails given datum, redeemer, and script context.
* `scriptSucceeds`, ensuring `Script` runs successfully.
* `scriptFails`, ensuring `Script` fails.
