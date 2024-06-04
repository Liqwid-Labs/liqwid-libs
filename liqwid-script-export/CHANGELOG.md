# Revision history for plutarch-script-export

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 2.3.0 -- 2023-01-11

### Added

- `stdout` exporter added. It is helpful as it doesn't require file IO.

### Modified

- `timeout` option added for `serve` option, default timeout is updated to 10 minuates.
- `file` option will now warn if it fails to look up the given file.

## 2.2.0 -- 2022-11-29

### Modified

- Now supports subcommand system
- File export reqires user to select specific builder to use.
- Add a subcommand to list all builders
- Dropped Git revesion support that was broken

## 2.1.0 -- 2022-11-02

### Added

- `RoledScript` is added to encode role data into `ScriptExport`.
- Aeson instances and other functions are updated to use `RoledScript`.
- Helper function `toRoledScript` is added

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
