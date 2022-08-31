# `liqwid-plutarch-extra` (aka LPE)

Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-plutarch-extra

## What is this?

LPE is a repository for holding Plutarch types, typeclasses, helpers, and utilities 
shared between [Liqwid Labs](https://github.com/Liqwid-Labs) projects.

## What _exactly_ does this do for me?

LPE is _primarily_ intended as a temporary destination for code before upstreaming to
[`plutarch-extra`](https://github.com/Plutonomicon/plutarch-plutus/tree/master/plutarch-extra). We welcome contributions, issues, or requests from the community, but first
encourage direct contributions to `plutarch`.

The code in LPE can broadly be categorized as follows:

- (Co)category typeclasses (`PFunctor` and friends),
- Missing functionality from existing Plutarch modules like `Plutarch.List`, `Plutarch.Maybe`, and so forth
- Genuine extras, like those found in `Precompile`, `Tagged`, `MultiSig`,
  and `FixedDecimal`

### Caveat Emptor

Because this code is the first destination for Liqwid Labs's projects to 
share snippets, the code in LPE may:
 
 - Be untested
 - Be undocumented
 - Be sub-optimal in terms of performance or readability
 - Duplicate functionality found elsewhere
 - Go against some notion of best practice
 - Disappear at a moment's notice, either because its broken, not needed,
   or has been upstreamed.

_Some_ functionality may be contraversial or make certain stylistic assumptions
that the authors of Plutarch don't share, and thus certain parts of LPE may never
be accepted into `plutarch-extra`. 

Users are welcome to inquire about the status of particular snippets on the [discord](https://discord.gg/yGkjxrYueB).

# How do I use this?

To integrate this with your project, use Nix. We work against the main branch of Plutarch, so you will have to use it also. See the `flake.nix` file for more details.

Liqwid uses [`liqwid-nix`](https://github.com/Liqwid-Labs/liqwid-nix) to set up
its projects.  You are not required to use `liqwid-nix` to use this library, but
you are welcome to check check it out.

# Standards

LPE code base follows a strict standards to increase consistency, to minimize
legacy impacts, to use proper automated means, and more. The standard document 
can be discovered in [here](https://liqwid.notion.site/Coding-Standards-cd3c430e6e444fa292ecc3c57b7d95eb).

# What can I do with this?

`liqwid-plutarch-extra` is licensed under the Apache 2.0 license (SPDX code
`Apache-2.0`); please see the `LICENSE` file for more details.
