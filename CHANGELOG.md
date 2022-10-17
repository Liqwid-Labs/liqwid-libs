# Revision history for plutarch-script-export

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 2.0.0 -- 2022-10-12

### Added

- `ScriptExport`, `RawScriptExport`, and `Linker` provides new standardized data structure for script exportation.
- Utility functions for new `ScriptExport` types.
- `insertStaticBuilder`, and `insertScriptExportWithLinker` was added.
- `exportMain` is new entry point for the script exporting program.
- `exportFile` exports scripts to file.
- Other internal data structure changed to better support new `ScriptExport` types.

## 1.1.1 -- 2022-09-15

### Added

- `mkStakeValidatorInfo` for creating `ScriptInfo`s from stake validators

## 1.1.0 -- 2022-09-14

### Modified

- Bumped types to Plutus V2. Bumped plutarch.

## 1.0.0 -- 2022-06-30

### Added

* First release
