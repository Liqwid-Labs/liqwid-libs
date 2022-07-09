module Test.Benchmark.Main (benchMain) where

import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt
import Path (Dir, Path, Rel, parseRelDir)
import System.Directory (createDirectoryIfMissing)

newtype Options = Options
  { output :: FilePath
  }

outputOpt :: Opt.Parser FilePath
outputOpt =
  Opt.strOption
    ( Opt.long "output-path"
        <> Opt.short 'o'
        <> Opt.metavar "OUTPUT_PATH"
        <> Opt.value "./benchmark-output"
        <> Opt.help "The output directory for writing benchmark results."
    )

benchOpt :: Opt.Parser Options
benchOpt = Options <$> outputOpt

parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (benchOpt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Run benchmarks, write results to output directory."
        )

{- | Passes the benchmark output directory to the argument.

 Ensures that the output dir exists and is a dir.
-}
benchMain :: (Path Rel Dir -> IO ()) -> IO ()
benchMain run = do
  options <- parseOptions
  -- This will thrown an error if it exists and is not a dir
  -- True to create parent dirs
  createDirectoryIfMissing True options.output
  dir <- parseRelDir options.output
  run dir

{- TODO
  The whole interface is rather low-level for now.
  A high-level interface needs to:
  - allow grouping benchmarks in a tree
  - allow to filter the tree with command line arguments, just like Tasty/QuickCheck do
  - needs to keep track of dependencies: a comparison of multiple
    benchmark-outputs needs to make sure those benchmarks are run, no matter what
    the filters were, but the benchmarks shouldn't be run more than once
  - write out metadata for the impls: script hashes, commit hashes, generation time
-}