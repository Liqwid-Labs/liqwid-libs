# liqwid-libs

A monorepo for Liqwid Labs plutarch libraries.

Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-libs.
Issues and project-management-related information are tracked on [Notion](https://www.notion.so/liqwid/).

## Overview

| Library                                                | Description                                                                                 |
|--------------------------------------------------------|---------------------------------------------------------------------------------------------|
| [liqwid-plutarch-extra](./liqwid-plutarch-extra)       | Collection of Plutarch types, typeclasses, helpers, and utilities  shared between projects. |
| [liqwid-script-export](./liqwid-script-export)         | Export scripts for off-chain consumption through a HTTP server.                             |
| [plutarch-quickcheck](./plutarch-quickcheck)           | Helper library to write Plutarch-oriented QuickCheck property tests.                        |
| [plutarch-context-builder](./plutarch-context-builder) | Helpful builders for `ScriptContext`s.                                                      |
| [plutarch-benchmark](./plutarch-benchmark)             | Library to compare algorithm implementations of Plutarch code.                              |
| [plutarch-unit](./plutarch-unit)                       | Library to write Plutarch-oriented unit tests using `tasty`.                                |

## Organization

Please read the README of the library you are trying to use. Versioning and changelogs are kept on a per-library basis.

## Importing using `liqwid-nix`

When importing these libraries using [`liqwid-nix`](https://github.com/Liqwid-Labs/liqwid-nix), you can use `extraHackageDeps` to specify which packages to include:

```nix
"${inputs.liqwid-libs}/plutarch-quickcheck"
"${inputs.liqwid-libs}/plutarch-context-builder"
```

This will include plutarch-quickcheck and plutarch-context-builder and make them available to cabal.
