# Revision history for `plutarch-benchmark`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

This repository was factored out of XplorerDAO in early August 2022.

## 2.0.0 -- 2024-05-06

Internally:

- Forked the library from [`liqwid-libs`](https://github.com/Liqwid-Labs/liqwid-libs)
- Bumped `base` version from `^>=4.16` to `^>=4.18.1.0`.
- The nix setup changed significantly, removing the dependency on `liqwid-nix`

These should not have an effect on the public-facing API, but are mentioned for completeness.

### Changed

- Added `Plutarch.Benchmark.Orphans` to give a `deriving newtype instance NFData UPLC.Size`

## 1.0.0 -- 2022-08-13

### Added

 - First Release
