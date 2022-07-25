# Revision history for `plutarch-context-builder`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

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
