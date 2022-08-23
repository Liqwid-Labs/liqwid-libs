# `plutarch-benchmark`
Public discussion and assistance can be found on [discord](https://discord.gg/yGkjxrYueB) @ #liqwid-libs

## What is this?

A helper library to compare algorithm implementations of Plutarch code.

Some of the benchmarking techniques are multi-threaded.

This library helps users to write benchmarks on memory units
and CPU units, and ranks those implementations in terms of best, 
worst, or average cases, across multiple sizes of inputs.

## What _exactly_ does this do for me?

The Plutus costing model assigns "CPU" and "memory" to scripts in a manner that is
[completely unlike traditional CPU and Memory] (https://www.notion.so/How-Execution-Budgeting-works-for-Onchain-scripts-a62e4a2cf37f4e7f8e0e47735f16048b). In addition, the budget and size limits of Plutus scripts
means that [Big-O] asymptotic behavior doesn't tell the full story: the constant factors will matter. 

This can lead to non-obvious performance tradeoffs between differing 
implementations of algorithms. This library provides tools to measure
performance empirically. Users can specify generators to generate 
test data up to certain sizes (either exhaustively or pseudo-randomly, with
coverage information included) and compare multiple implementations side-by-side.

Once benchmarks and generators are defined, the library executes the benchmarks
in parallel and writes the results to CSV.

# How do I use this?

There are two styles of benchmarking made available by the library.
The `example/Main.hs` module demostrates the basic benchmarking process,
with the key elements being described below. Users new to the library can read 
the examples as code, as well as execute the benchmarks with `cabal bench` to see 
what they do.

The `Plutarch.Benchmark.Sized` module exposes a types and functions 
to assist with generating test data. The key types are:

- `SSample` (for "Sized Sample), which holds sample data along with
  metadata about the size of the inputs it was generated from, 
  the size of the sample itself, coverage, and the sample itself
- `SUniversalGen` (for "Sized Universal Generator"), which holds
  a pair of generators: exhaustive generators for small input sizes,
  as well as random generators for large input sizes.
  
Using these types and the `bench*` functions exposed in the module (as well as 
those exposed in `Plutarch.Bench.Plutarch`), users can generate benchmarks in 
different ways to account for different generator distributions, input sizes, 
and coverage needs.

Individual benchmarks for a single implementation can be written to CSV using 
the `writePerAxisCSVs` function from `Plutarch.Benchmark.Cost`, and the same
module exposes the `writeComparisonPerAxisCSVs` to group multiple benchmarks 
into a single CSV and rank them according to performance.

To integrate this with your project, use Nix. We work against the `master`
branch of [Plutarch](https://github.com/Plutonomicon/plutarch-plutus), so you 
will have to use it also. See the [`flake.nix`](./flake.nix) file for more details.

# What can I do with this?

`plutarch-benchmark` is licensed under the Apache 2.0 license (SPDX code
`Apache-2.0`); please see the `LICENSE` file for more details.
