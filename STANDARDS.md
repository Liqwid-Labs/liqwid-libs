## Introduction

We describe a set of standards for code. We also explain our reasoning for these
choices. We want this to act as a living document of our practices for current,
and future, contributors to the project. We also intend this document to evolve
as our needs change, as well as act as a single point of truth for standards.

## Motivation

We define our desired outcomes from our choice of practices.

### Increased consistency

Inconsistency is worse than _any_ standard. It requires us to track a large
amount of case-specific information without any logic to it, complicates
becoming familiar with the codebase for newcomers, works _against_ intuition,
and generally leads to friction, frustration and ultimately, worse results.
Software development is already difficult enough: not only are many of our
problems precise and detail-oriented, we also have to deal with _decades_ of
poor choices foisted upon us, over which we have no control at all.

Thus, we strive to be _automatically consistent_. Similar things should look
similar; different things should look different. As much as possible, we must
pick some rules and _stick to them_; furthermore, said rules must be clear,
explicit and well-motivated. This will ultimately benefit us, both in the short
and long term. The standards described in this document, and indeed, the
document itself, are written with this foremost in mind.

### Limited non-local information

There is a limited amount of space in a developer's skull. Everyone, no matter
how experienced or skilled, has bad days, and we forget things, or make decision
that are not ideal in the short or long term. Therefore, we need to limit our
cognitive load: by giving ourselves less to think about, we reduce the amount of
trouble we can get ourselves into due to the aforementioned skull limitations. 

One of the worst contributors to cognitive load, after inconsistency, is
_non-local information_: that is, the requirement to have some knowledge or
understanding of matters beyond the current scope of work. That scope of work
ranges from a data type, to a module, to a whole project; in all cases, the more
non-local information we require ourselves to hold in our minds, the less space
that leaves to solving the problems, or doing the task, we actually have at
hand, and the more errors and bad choices we introduce in the process.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided, and we should have as much locality of
information as possible, whenever possible. Our work should be broken down into
discrete, minimal and logical units, which can be analyzed, worked on, reviewed
and tested in as much isolation as possible. This also applies to our external
dependencies.

We make many of the choices described in this document around limiting the
amount of non-local information required at all levels of a codebase.
Additionally, we aim to proscribe doing certain things 'just because we can' in
a way that would be difficult for other Haskellers to follow, regardless of
skill level, for similar reasons: a lot of such 'because we can' techniques
require a lot of background to understand, which not every developer necessarily
has, or can recall easily.

### Minimized legacy impact

Haskell is a language older than many of the people writing it, and many parts
of its ecosystem _definitely_ look their age. Age brings legacy, much of which
is based on decisions we now know to be bad or ill-considered in retrospect. We
cannot escape our history, but we can minimize its impact on our current work.

In light of this, we use this document to describe _today's_ good practices. We
also aim to avoid 'sharp edges' by proscribing them away in a principled,
consistent and justifiable manner.

### Reduction of drudgery by automatic means

As developers, we should use our tools to make ourselves as productive as
possible. There is no reason for us to do a task if a machine can do it for us,
specially when this task is boring or repetitive. We love Haskell as a language
not least of all for its capability to abstract, to describe, and to make _fun_
what other languages make _dull_ or _impossible_; likewise, our work must do the
same.

Many of the tool-related proscriptions and requirements of this document are
driven by the desire to remove boring, repetitive tasks that don't need a human
to complete. By removing the need to think about such things, we can focus on
the tasks that _do_ need a human to complete; thus, we get more done, quicker,
with less effort.

## Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC 2119](https://tools.ietf.org/html/rfc2119).

## Tools

### Compiler warning settings

The following warnings MUST be enabled for all builds of any project, in the
`ghc-options` of the Cabal file:

* `-Wall`
* `-Wcompat`
* `-Wincomplete-uni-patterns`
* `-Wincomplete-record-updates`
* `-Wredundant-constraints`
* `-Wmissing-export-lists`
* `-Wmissing-deriving-strategies`
* `-Werror`

Additionally, `-Wredundant-constraints` SHOULD be enabled for all builds of any
project, in the `ghc-options` of the Cabal file. Exceptions are allowed when the
additional constraints are designed to ensure safety, rather than due to
reliance on any method. If this warning is to be disabled, it MUST be disabled
in the narrowest possible scope: this SHOULD be a single module.

#### Justification

Most of these options are suggested by 
[Alexis King](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build)
- the justifications for them can be found at the link. These fit well with 
our motivations, and thus, should be used everywhere. `-Werror` ensures that 
warnings _cannot_ be ignored: this means that problems get fixed sooner. We 
also add `-Wmissing-export-lists` and `-Wmissing-deriving-strategies`: the 
first ensures that we clearly indicate what is, and isn't, part of a module's 
public API, and the second ensures that we have clarity about how everything 
is derived. As we mandate both export lists and deriving strategies in this 
document, these warnings ensure compliance, as well as checking it 
automatically.

### Linting

Every source file MUST be free of warnings as produced by
[HLint](http://hackage.haskell.org/package/hlint). The CI for any project MUST
enforce this. 

HLint warnings MUST be disabled by annotation use on a per-module basis when
enforcing the recommendation would cause the code to no longer compile.

#### Justification

HLint automates away the detection of many common sources of boilerplate and 
inefficiency. It also describes many useful refactors, which in many cases 
make the code easier to read and understand. As this is fully automatic, it 
saves effort on our part, and ensures consistency across the codebase without 
us having to think about it.

There are, however, some cases where HLint isn't as aware of GHC features and
their interactions as we would like. One common example is that sometimes, full
eta-expansion is required for GHC's type checker to accept some code, but HLint
will argue for a reduction instead. In this case, disabling HLint warnings using
annotations is the right choice, but this would usually only be in a narrow
scope, and hence doesn't justify configuration changes project-wide. This also
acts as a good signpost, as this behaviour can be unexpected for Haskell
developers as well. 

### Code formatting

Every Haskell source file MUST be formatted according to
[Fourmolu](http://hackage.haskell.org/package/fourmolu), with the following
settings (as per its settings file):

* `indentation: 2`
* `comma-style: leading`
* `record-brace-space: true`
* `indent-wheres: true`
* `diff-friendly-import-export: true`
* `respectful: true`
* `haddock-style: multi-line`
* `newlines-between-decls: 1`

Each source code line MUST be at most 80 characters wide.

The project's Cabal file MUST be formatted according to
[`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt).

#### Justification

Consistency is the most important part of readable codebases. Having a single,
automatically-enforced, standard means that we can be sure everything will look
similar, and not have to spend time or mind-space ensuring that our code
complies with said standard. This also helps with `git diff`s, as the difference
'spreads around' less.

Lines wider than 80 characters become difficult to read, especially when viewed
on a split screen. An 80-character maximum is also a long-standing convention,
both within Haskell and also elsewhere. Lastly, very long lines tend to mean
that we need better naming or refactoring.

### CI

All projects MUST have CI. The CI MUST ensure the following:

* All stanzas in the project compile; namely, that the equivalent of `cabal
  new-build --enable-tests --enable-benchmarks` completes without error.
* The formatting requirements described in 
  [the code formatting section](#code-formatting) are enforced for both Haskell
  files and the Cabal file.
* The linting requirements described in [the linting section](#linting) are
  enforced.

The CI SHOULD also ensure that any test stanzas pass; namely, that the
equivalent of `cabal new-test` completes without error. Exceptions are allowed
when the project lacks tests.

#### Justification

CI is an important tool in modern software development practice. It ensures that
reviewers don't have to worry about machine-checkable issues, helps hold up
standards, and can potentially alert us to issues that arise outside of a
specific developer's machine. Having the CI not only build the project, but also
run its tests, can help ensure that we don't accidentally create regressions,
and also reduces reviewer cognitive load.

## Code practices

### Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as
'JSON', 'API', etc) SHOULD be downcased; thus, `repairJson` and
`fromHttpService` are correct. Exceptions are allowed for external libraries
(for example, Aeson's `parseJSON`).

Plutarch identifiers MUST be prefixed with 'p' without affecting the case of 
the subsequent character. Thus, Plutarch types are named `PFoo`, while Plutarch
functions are named `pfoo`.

#### Justification

camelCase for non-type, non-data-constructor names is a long-standing convention
in Haskell; in fact, HLint even checks for this. TitleCase for type names or
data constructors is mandated by the language itself. Obeying these conventions
reduces cognitive load, as it is common practice across the entire Haskell
ecosystem, and thus easily familiar to anyone who's written or read any Haskell.
There is no particular standard regarding acronym casing: examples of always
upcasing exist (Aeson), but there are also examples of downcasing
(`http-api-data`). One choice for consistency should be made: we choose
downcasing as it is easier to type.

The naming convention for Plutarch identifiers mirrors Plutarch itself, and also
acts as a useful visual aid to separating on-chain and off-chain functionality.
As Plutarch is a DSL, these two kinds of identifiers can often mix in the same
module, and typically may have 'correspondences' between one another: having a
clear convention to distinguish which is which saves on head-scratching for
naming, and also makes understanding what a module is doing much simpler.

### Modules

#### Imports

All modules MUST use only the following conventions for imports:

* `import Foo (Baz (Quux, quux), Bar, frob)`
* `import qualified Bar.Foo as Baz`

More precisely, imports must _either_ specify each identifier they import, or
qualify the import.

Some specific examples follow. Type class methods SHOULD be imported alongside
their class:

```haskell
import Control.Applicative (Alternative ((<|>)))
```

An exception is given when you only need the method:

```haskell
import Control.Applicative (mempty)
```

Record fields MUST be imported alongside their record:

```haskell
Import Data.Monoid (Endo (appEndo))
```

Data types from modules imported qualified SHOULD be imported unqualified by
themselves:

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as Vector
```

An exception is given if such an import would cause a name clash:

```haskell
-- no way to import both of these without clashing on the Vector type name
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

-- We now use Boxed.Vector to refer to Vector from Data.Vector, and 
-- Storable.Vector otherwise
```

We also permit an exception to use a 'hiding import' to replace part of the
`Prelude`:

```haskell
-- replace the String-based readFile with a Text-based one
import Prelude hiding (readFile)
import Data.Text.IO (readFile)
```

Data constructors MUST be imported individually. For example, given the
following data type declaration:

```haskell
module Quux (Foo (Bar, Baz)) where

data Foo = Bar Int | Baz
```

Its corresponding import (assuming only the `Bar` constructor is needed
alongside the type name) is:

```haskell
import Quux (Foo (Bar))
```

Qualified imports SHOULD use their entire module name (that is, the last
component of its hierarchical name) as the qualified prefix. For example:

```haskell
import qualified Data.Vector as Vector
```

Exceptions are granted when:

* Such an import would cause a name clash (for example, different `vector`
  modules); or
* We have to import a data type qualified as well.

Qualified imports of multiple modules MUST NOT be imported under the same name.
Thus, the following is wrong:

```haskell
-- Do not do this!
import qualified Foo.Bar as Baz
import qualified Foo.Quux as Baz
```

#### Justification

One of the biggest challenges when reading, or making sense of, a module which
depends on other modules, whether those are part of the same project or an
external dependency, is knowing where a given identifier's definition can be
found. Having explicit imports as described here helps make this search as
straightforward as possible: just looking at the imports should be enough. This
also limits cognitive load when examining the sources of what we import: if we
don't import an identifier, we generally don't need to worry about it. Lastly,
being explicit in this way avoids 'name theft', where many useful names get
'used up' by dependencies.

In general, type names occur far more often in code than function calls: we have
to use a type name every time we write a type signature, but most functions get
used only a few times. Thus, we want to reduce the amount of extra noise needed
to write a type name if possible. Additionally, name clashes from function names
are far more likely than name clashes from type names: for example, consider how
many types on which a `size` function makes sense to define. Thus, importing
type names unqualified, even if the rest of the module import is qualified, is
good practice and saves a lot of prefixing.

Multi-imports under the same qualification is arguably a severe misfeature: in
general, qualified imports must uniquely identify the module that the identifier
comes from to be useful. In any case, this leads to considerable confusion, as
now, to determine the source of an identifier requires checking multiple
modules, rather than just one, and there's no good reason to do this.

#### Exports

All modules MUST have explicit export lists; that is, every module must
explicitly state what exactly it exports. Export lists SHOULD be separated using
Haddock headings:

```haskell
module Foo.Bar (
  -- * Types
  Baz,
  Quux (..),
  -- * Construction
  mkBaz,
  quuxFromBaz,
  -- etc
  ) where
```

An exception is granted if Haddocks would not be required for a module according
to these standards.

In the specific case where a module only provides instances ('compatibility
orphans' for example), the export list MUST be empty.

A module MUST NOT re-export identifiers from outside of the project: for
example, the following is not allowed:

```haskell
module Foo.Bar (
  Value
  ) where

import Data.Aeson (Value)
```

If a module re-exports identifiers from a different module in the same project,
the identifiers MUST be exported individually; thus, `module` exports are not
allowed. Furthermore, re-exporting re-exports MUST NOT be done; you can only
export something you define directly, or re-export something defined directly by
a module you import.

#### Justification

Explicit export lists are an immediate, clear and obvious indication of what
public interface a module provides. It gives us stability guarantees (namely, we
know that we can change things that aren't exported without breaking downstream
code at compile time), and tells us where to go looking first when inspecting or
learning the module. Additionally, it means there is less change that
implementation details 'leak' out of the module due to mistakes on the part of
developers, especially those who may not be familiar with the code in question.

Allowing wildcard _exports_ while disallowing wildcard _imports_ is justified on
grounds of locality of information. Seeing a wildcard import of all a type's
data constructors or fields doesn't tell us what these _are_ without looking up
the module from where they are exported; having such an import be explicit
reduces how much searching we have to do, as well as telling us clearly where
certain identifiers come from. However, if we are reading an export list, we
have the type definition in the same file we're already looking at, making it
fairly easy to check.

Re-exports are a useful feature, allowing us to have 'internal' versus
'external' modules, which make for useful interface stability guarantees.
However, _transitive_ re-exports are an unnecessary obfuscation, both for people
familiarizing themselves with the code, and people trying to use the code (if
it's a library). Having to follow a daisy chain of multiple re-exports to get to
a definition is tedious, and ultimately not necessary: the only distinction
required are 'internal' modules and 'external' modules, where 'external' modules
import 'internal' modules, and then selectively re-export. The transitive
re-export issue counts _double_ when dealing with identifiers from outside the
project: in addition to being even more tedious, this is arguably an abstraction
boundary violation. We forbid `module` re-exports on the same grounds as we
restrict imports: without knowing exactly what we're re-exporting, it makes it
very difficult to see what exact public interface we are presenting to the
world.

### ``LANGUAGE`` pragmata

The following pragmata MUST be enabled at the project level (that is, in the
Cabal file, in the `default-extensions` section):

* ``BangPatterns``
* ``BinaryLiterals``
* ``ConstraintKinds``
* ``DataKinds``
* ``DeriveTraversable``
* ``DerivingVia``
* ``EmptyCase``
* ``FlexibleContexts``
* ``FlexibleInstances``
* ``GeneralisedNewtypeDeriving``
* ``HexFloatLiterals``
* ``InstanceSigs``
* ``KindSignatures``
* ``LambdaCase``
* ``MultiParamTypeClasses``
* ``NumericUnderscores``
* ``OverloadedStrings``
* ``ScopedTypeVariables``
* ``StandaloneDeriving``
* ``TupleSections``
* ``TypeOperators``

All other `LANGUAGE` pragmata MUST be enabled per-file. All `LANGUAGE` pragmata
MUST be placed at the top of the source file, written as `{-# LANGUAGE
PragmaName #-}`, with one `LANGUAGE` pragma per line.

Furthermore, the following pragmata MUST NOT be used, or enabled, anywhere:

* `DeriveDataTypeable`
* `PartialTypeSignatures`
* `PostfixOperators`

#### Justification

Our choice of default extensions is primarily driven by the following
considerations:

* How useful, or widely-used, is this extension?
* Does this extension support other priorities or requirements from our
  standards?
* Does this extension iron out a bad legacy of the language, or does it extend 
  the language in new (and surprising) ways?
* Does it make Haskell behave similarly to other languages, particularly in
  smaller or narrower-context issues of syntax?
* Is this extension something that needs 'signposting', in that it involves
  additional thought or context that wouldn't be needed otherwise?

Extensions that are widely-used or highly useful, iron out bad legacy, 
increase similarity in certain syntactical features common to many languages, 
and that don't require signposting are the primary candidates for inclusion by
default. Additionally, some other standards defined in this document mandate
certain extensions anyway: in this case, having them on by default saves us on
having to enable them per-module, which we would have to do otherwise.

`BangPatterns` are a much more convenient way to force evaluation than
repeatedly using `seq`. They are not confusing, and are considered ubiquitous 
enough for inclusion into the `GHC2021` standard. Having this extension on by
default simplifies a lot of performance tuning work, and doesn't really require
signposting.

`BinaryLiterals`, `HexFloatLiterals` and `NumericUnderscores` all simulate
syntactic features that are found in many (or even _most_) other programming
languages. Furthermore, these syntactic features are extremely convenient in
many settings, ranging from dealing with large numbers to bit-twiddling. If
anything, it is _more_ surprising and annoying when these extensions _aren't_
enabled, and arguably should have been part of Haskell's syntax from the
beginning. Enabling these extensions project-wide actually encourages better
practices and readability, and costs almost nothing.

The `Constraint` kind is not in Haskell2010, and thus, isn't recognized by
default. While working with constraints as first-class objects isn't needed
often, this is much more common in Plutarch: for example, the `PIsList` type
class requires defining an associated type synonym involving the `Constraint`
kind. Furthermore, we require explicit kind and type signatures as part of this
standard, which makes enabling this a requirement. There is no harm in enabling
this globally: other 'rich kinds' such as `Symbol` or `Nat` don't require any
extensions for their use, and whether you enable this extension or not changes
no behaviour (`Constraint` exists whether you enable this extension or not).

`DerivingVia` provides two benefits. Firstly, it implies
`DerivingStrategies`, which is good practice to use (and in fact, is required by
this document, and checked by [mandatory warnings](#compiler-warning-settings)): 
this avoid ambiguities between different derivations, and makes the intent of 
a derivation clear on immediate reading. This reduces the amount of non-local 
information about derivation priorities. Secondly, `DerivingVia`
enables _considerable_ savings in boilerplate in combination with other
extensions that we enable either directly or by implication. While technically,
only `DerivingStrategies` would be sufficient for our requirements, since
`DerivingVia` is not mandatory and is clearly signposted, while having no
effects beyond its use sites, we enable it globally for its usefulness.

`DeriveTraversable`, together with `GeneralizedNewtypeDeriving`, allows us
considerable savings on boilerplate. Firstly, it allows a `stock` derivation of
`Functor`: this is completely safe and straightforward, due to
[parametricity](https://www.schoolofhaskell.com/user/edwardk/snippets/fmap), and
saves us effort in many cases. Secondly, `GeneralizedNewtypeDeriving` allow us
to 'borrow' any instance from the 'underlying' type: this is also completely
safe and straightforward, as there is no ambiguity as to what that instance
could be. This allows powerful examples such as this:

```haskell
newtype FooM (a :: Type) = FooM (ReaderT Int (StateT Text IO) a)
   deriving newtype (
      Functor,
      Applicative,
      Monad,
      MonadReader Int,
      MonadState Text,
      MonadIO
      )
```

The savings in code length alone make this worthwhile; in combination with their
locality and good behaviour, as well as their lawfulness, this makes them a good
candidate for being always on. `DeriveTraversable` is, in part, required to
solve a problem of [not being able to `coerce` through a
`Functor`](https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/).
While `Traversable` is lawful, it is _not_ lawful to the same degree `Functor`
is, and multiple implementations in many cases are possible. This combination of
factors makes even `newtype` or `via` derivations of `Traversable` impossible,
requiring special support from GHC, which is exactly what `DeriveTraversable`
permits. This is a historically-motivated inconsistency in GHC which should
really not exist at all: while this only papers over the problem (as with this
extension, only `stock` derivations become possible), it at least means such
derivations can be done at all. Having this globally enabled makes this
inconsistency slightly less visible, and due to `Traversable`'s lawfulness, is
completely safe. While this merely provides a derivation for _a_ lawful
`Traversable`, rather than _the_ lawful traversable, because of its lawfulness,
this is completely safe and requires no signposting.

`EmptyCase` resolves an inconsistency in Haskell2010, as the report allows us to
_define_ an empty data type (that is, one with no constructors), but not pattern
match on it. This should really be in the language, and enabling this globally
resolves a strange inconsistency in the language at no real cost.

`FlexibleContexts` and `FlexibleInstances` paper over a major deficiency in
Haskell2010, which isn't well-motivated in general. There is no real reason to
restrict type arguments to variables in either type class instances or
constraints: the reasons for this restriction in Haskell2010 are entirely for
the convenience of compiler writers. Having such a capability produces no
ambiguities, and in many ways, the fact that such things are _not_ allowed by
default is more surprising than anything. Furthermore, many core ecosystem
libraries, and even boot libraries (`mtl` being the most obvious example) rely
on one, or both, of these extensions being enabled. Enabling these globally is
both logical and harmless.

`InstanceSigs` are harmless by default, and introduce no complications. It is
in fact quite strange that by default, we _cannot_ put signatures on type class
methods. Furthermore, in the presence of type arguments, especially higher-rank
ones, such signatures are practically mandatory anyway. Enabling this is
harmless, very useful, and resolves a strange language inconsistency.

`KindSignatures` become extremely useful in any setting where 'exotic kinds'
(meaning, anything which isn't a composition of `Type` and `->` only) are
commonplace. Much like _type_ signatures clarify expectations and serve as
active documentation (even where GHC can infer them), explicit _kind_ signatures
serve the same purpose 'one level up'. When combined with our requirements for
explicit `forall`s for type variables, they simplify the usage of 'exotic
kinds', and provide additional help from both the type checker and the code. One
major reason to want 'exotic kinds' is Plutarch: even the most basic Plutarch
entity involves dealing with the kind `S -> Type`. Since we would have to enable
this extension anyway just to comply with standards, having it on by default is
sensible.

`LambdaCase` reduces a lot of code in the common case of analysis of
single sum type values as function arguments. Without this extension, we either
have to write a dummy `case` statement:

```haskell
foo s = case s of 
  -- rest of code here
```

Or, alternatively, we need multiple heads:

```haskell
foo Bar = -- the Bar case
foo (Baz x y) = -- the Baz case here
-- etc
```

`LambdaCase` is shorter than both of these, and avoids us having to bind a
variable only to pattern match it away immediately. It is convenient, clear from
context (especially with our requirement for explicit type signatures), and
doesn't cause any bad interactions.

`MultiParamTypeClasses` are required for a large number of common Haskell
libraries, including `mtl` and `vector`, and in many situations. Almost any
project of non-trivial size must have this extension enabled _somewhere_, and if
the code makes significant use of `mtl`-style monad transformers, or defines
anything non-trivial for `vector`, `MultiParamTypeClasses` must be enabled.
Furthermore, Plutarch makes use of multiple type classes with multiple
parameters, such as `PTryFrom`, which also require `MultiParamTypeClasses` to be
on. Additionally, the original restriction in Haskell2010 solved by this
extension is, much like `FlexibleInstances` and `FlexibleContexts`, put in place
for no reason other than the convenience of compiler writers. Lastly, although
having this extension enabled can introduce ambiguity into type checking, this
only applies in one of two situations:

* When we want to define multi-parameter type classes ourselves, which is rarely
  necessary; or
* If the multi-parameter type class we are trying to use has this problem, which
  we can't do much about anyway.

Enabling `MultiParamTypeClasses` globally is practically a necessity given all
of these, and is clear enough that it doesn't need signposting.

`OverloadedStrings` deals with the problem of `String` being a suboptimal choice
of string representation for basically _any_ problem, but at the same time being
forced on us by `base`. This has led to a range of replacements, of which `Text`
is generally the most advised; Plutarch also provides `PString` specifically for
on-chain use. Having `OverloadedStrings` enabled allows us to use string literal
syntax for both `Text` and `PString`, which is convenient for the first and
practically mandatory for the second; it is also fairly obvious in intent. It is
not, however, without problems:

* `ByteString`s are treated as ASCII strings by their `IsString` instance, which
  makes its string literal behaviour somewhat surprising;
* Overly polymorphic behaviour in many functions (especially in the presence of
  type classes) often forces either type signatures or type arguments; and
* `IsString` is basically lawless.

However, these problems (aside from the last one) aren't usually caused by
`OverloadedStrings` itself, but by other libraries and their implementations,
either of `IsString` itself, or overly-polymorphic use of type classes without
appropriate (or _any_ laws). A particularly egregious offender is `KeyValue`
from Aeson, which has all the above problems in spades. However, the convenience
of this extension, combined with the fact that for `PString` there basically
_isn't_ another way to form literals, makes this worth enabling by default.

`ScopedTypeVariables` needs to be enabled globally for several reasons. The
primary reason is, when combined with our requirement for explicit signatures 
for both type and kind arguments, _not_ having `ScopedTypeVariables` on would
produce _extremely_ weird behaviour. Consider the case below:

```haskell
foo :: a -> b
foo = bar . baz
   where
      bar :: String -> b
      bar = ...
      baz :: a -> String
      baz = ...
```

This would cause GHC to produce a _fresh_ `a` in the where-bind of `baz`, and a
_fresh_ `b` in the where-bind for `bar`. This is confusing and makes little
sense - if the user wanted a fresh type variable, they would have named it that
way. Worse still, if this code fails to type check due to errors in the
where-binds, the type error makes very little sense, except for those who have
learned to spot this error. This becomes _really_ bad when the type variable is
constrained:

```haskell
foo :: (Monoid m) => m -> String
foo = bar . baz
   where
      baz :: m -> Int
      baz = ... -- this has no idea that m is a Monoid, since m is fresh!
```

Furthermore, with the availability of `TypeApplications`, as well as possible
ambiguities stemming from multi-parameter type classes (which Plutarch has some
of), we need to know the order of type variable application. While there _is_ a
default, having to remember it, especially in the presence of type class
methods, is tedious and error-prone. This becomes even worse when dealing with
skolems, which Plutarch often requires us to do, since GHC cannot in general
properly infer them. Explicit ordering of type variables for this purpose can
_only_ be done with `ScopedTypeVariables` enabled. 

Lastly, it could be argued that `ScopedTypeVariables` is the way Haskell ought
to work in the first place. If we name a _value_, it propagates 'downward' into
where-binds - why should _types_ work any differently? The default of 'silently
refreshing' type variables is thus both surprising and counter-intuitive. All of 
these, together with our requirement for explicit signatures, make having
`ScopedTypeVariables` on by default inevitable, and arguably even the right 
thing to do.

`StandaloneDeriving`, while not being needed often, is quite useful when using
`via`-derivations with complex constraints, such as those driven by type
families, or for GADTs. This can pose some syntactic difficulties (especially
with `via` derivations), but the extension is not problematic in and of itself,
as it doesn't really change how the language works, and is clearly
self-signposting. Having `StandaloneDeriving` enabled by default is thus not
problematic.

`TupleSections` smooths out an oddity in the syntax of Haskell2010 regarding
partial application of tuple constructors. Given a data type like this:

```haskell
data Pair a = Pair a a
```

We accept it as natural that we can partially-apply `Pair` by writing `Pair
"foo"` to get something of type `String -> Pair String`. However, without
`TupleSections`, the same does not extend to tuple constructors. As special
cases are annoying to keep track of, and this particular special case serves no
purpose, it makes sense to enable `TupleSections` by default. This also smooths
out an inconsistency that doesn't apply to anything else.

`TypeOperators` is practically a necessity when doing any kind of type-level
work in Haskell. Firstly, much in the same way infix data constructors are
extremely useful, and sometimes clearer than their prefix forms, infix _type_
constructors serve a similar function. Secondly, Plutarch requires a type
operator to do almost anything useful, as the type of a Plutarch function
involves `:-->`, which is a type operator. Thus, we would likely have to enable
this almost anywhere _anyway_, and given its similarity to data constructors,
this seems clear in its intent.

We exclude `DeriveDataTypeable`, as `Data` is a strictly-worse legacy version of
`Generic`, while `Typeable` derivations are not needed anymore. The only reason
to enable this extension, and indeed, use either derivation, is for
compatibility with legacy libraries, which we don't need any of, and the number
of which shrinks every year. Furthermore, `Data` is conflictingly named with the
`Data` from PlutusCore, which we _do_ need for defining `PConstantDecl` at
least. If we're using this extension at all, it's probably a mistake.

`PartialTypeSignatures` is a misfeature. Allowing leaving in type holes (to be
filled by GHC's inference algorithm) is an anti-pattern for the exact same
reasons as not providing top-level type signatures: while it is (mostly)
possible for GHC to infer signatures, we lose considerable clarity and active
documentation by doing so, in return for (quite minor) convenience. While the
use of typed holes during _development_ is a good practice, they should not
remain in the final code: to make matters worse, `PartialTypeSignatures`
actually works _against_ typed-hole-driven development, as once GHC has enough
information to infer the hole, it won't emit any more information. Furthermore,
once you start to engage with higher-rank types (which Plutarch practically
requires), or indeed, practically anything non-trivial at the type level, GHC's
inference for such holes is often wrong, if it's decidable at all. There is no
reason to leave behind typed holes instead of filling them in, and we shouldn't
encourage this.

`PostfixOperators` are arguably a misfeature. Infix operators already require a
range of special cases to support properly (such as what symbols create an infix
operator, how to import them at the value and type level, etc); postfix
operators make all these special cases even worse. Furthermore, postfix
operators are seldom, if ever, used, and typically aren't worth the trouble.
Haskell is not Forth, none of our dependencies rely on postfix operators, and
defining our own would create more problems than it would solve.

### Versioning

Projects MUST use the [PVP](https://pvp.haskell.org). Three, and only three,
version numbers MUST be used; two major, one minor.

#### Justification

The [Package Versioning Policy](https://pvp.haskell.org) is the conventional
Haskell versioning scheme, adopted by most packages on Hackage. It is clearly
described, and even automatically verifiable by use of tools like
[`policeman`](https://hackage.haskell.org/package/policeman). Adopting its use
is both in line with community standards (making it more familiar and easy to
remember), and simplifies cases such as Hackage publication and open-sourcing in
general.

Three version numbers (two major, and one minor) is the minimum allowed by the
PVP. As parsimony is best, and more granularity than this isn't generally
necessary, adopting this model is the right decision.

### Documentation

Every publically-exported definition MUST have a Haddock comment, detailing its
purpose. If the definition is a function, it SHOULD also have examples of use
using [Bird
tracks](https://haskell-haddock.readthedocs.io/en/latest/markup.html#code-blocks);
exceptions are allowed for functions whose name and purpose describe its usage
sufficiently to understand their use. The Haddock for a publically-exported
definition SHOULD also provide an explanation of any caveats, complexities of
its use, or any common issues a user is likely to encounter.

If the project is a library, these Haddock comments SHOULD carry an
[`@since`](https://haskell-haddock.readthedocs.io/en/latest/markup.html#since)
annotation, stating what version of the library they were introduced in, or the
last version where their functionality or type signature changed.

For type classes, their laws MUST be documented using a Haddock comment.

Each project repository MUST have a README, which explains how to build and use 
the application or library. If the project's Cabal file defines one or more
`executable` stanzas, the README should also explain how to run each executable,
including command-line arguments or options. For more complex projects,
additional documentation and code examples SHOULD be provided.

#### Justification

Code reading is a difficult task, especially when the 'why' rather than the
'how' of the code needs to be deduced. A good solution to this is documentation,
especially when this documentation specifies common issues, provides examples of
use, and generally states the rationale behind the definition. In an ideal
world, just reading the documentation should tell you everything you need to
know about how and why to use a given function or data type: reading their code
should only be required if modifying the definitions themselves.

For libraries, it is important to inform users what changed in a given version,
especially where 'major bumps' are concerned. While this would ideally be
addressed with accurate changelogging, it can be difficult to give proper
context in the setting of a changelog. `@since` annotations provide a granular
means to indicate the last time a definition changed considerably, allowing
someone to quickly determine whether a version change affects something they are
concerned with.

As stated elsewhere in our standards, type classes having laws is critical to
our ability to use equational reasoning, as well as a clear indication of what
instances are and aren't sensible. These laws must be clearly stated, as this
helps both those who want to understand the purpose of the type class, and also
those who want to understand its instances.

### Type and kind signatures

All module-level definitions, as well as `where`-binds, MUST have explicit type
signatures. Type variables MUST have an explicit `forall` scoping them, and all
type variables MUST have explicit kind signatures. Thus, the following is wrong:

```haskell
-- Do not do this
data Foo a = Bar | Baz [a]
 
quux :: (Monoid m) => [m] -> m -> m
```

Instead, these should look like this:

```haskell
data Foo (a :: Type) = Bar | Baz [a]

quux :: forall (m :: Type) . (Monoid m) => [m] -> m -> m
```

Each explicit type signature MUST correspond to one definition only. Thus, the
following is wrong:

```haskell
bar :: Int
baz :: Int
(bar, baz) = someOtherFunction someOtherValue
```

Instead, write it like this:

```haskell
bar :: Int
bar = fst . someOtherFunction $ someOtherValue

baz :: Int
baz = snd . someOtherFunction $ someOtherValue
```

#### Justification

Explicit type signatures for module-level definitions are a good practice in
Haskell for several reasons: they aid type-driven (and typed-hole-driven)
development by providing better compiler feedback; they act as 'active
documentation' describing what we expect a function to (and _not_ do); and they
help us plan and formulate our thoughts while we implement. While GHC can infer
_some_ type signatures, not having them significantly impedes readability, and
can easily go wrong in the presence of more advanced type-level features.

In the context of Plutarch, this takes on new significance, as Plutarch relies
on an `ST`-like construction for safety. This needs rank-2 polymorphism to work,
and GHC's inference is significantly weakened even in this (relatively
straightforward) case. This can show up even in situations as straightforward as
this one:

```haskell
pfoo :: Term s (PList a :--> a)
pfoo = phoistAcyclic $ plam $ \ell -> go $# puncons # ell
  where
    go self t = ...
```

In this situation, `go` has a skolemized type argument, but GHC will fair to
infer it. Moreover, the error you will get reads like complete gibberish
_unless_ you know exactly what to look for and what it means.

Furthermore, the existence of `TypeApplications` now requires us to care not
just about _which_ type variables we have, but also the _order_ in which they
are defined. While there is an algorithm for determining this precisely, this
leads to three unpleasant consequences:

* Those trying to use our definitions with `TypeApplications` now have to
  remember the algorithm. Its interactions, especially with type classes, are
  not very straightforward, and typically quite tedious to work through.
* An invisible change at the value level (such as reordering constraints) can be
  an API break.
* The type variables that need to be applied with `TypeApplications` may not be
  well-positioned in declaration order, requiring long 'snail trains' of
  inference holes `@_`.

We avoid _all_ of these problems by explicitly `forall`ing all of our type
variables: reading the type signature is now sufficient to determine what order
is in use, changes in that order are stable, and we can choose an optimal
ordering with `TypeApplications` in mind. This is not only convenient, but also
much easier to understand, and much less brittle.

On top of this, Plutarch, along with most non-trivial work at the type level,
requires us to use 'exotic kinds' (any kind which isn't some combination of only
`Type` and `->`): Plutarch uses `S -> Type`, and variants of this frequently,
and also requires use of `Constraint`, to name but a few. Once again, GHC _can_
infer kinds most of the time, but, for much the same reasons we prefer explicit
type signatures, explicit _kind_ signatures allow us to indicate clearly to our
users what kinds we expect, as well as ensuring that we don't make any errors
ourselves. This, together with explicit `forall`s, essentially brings to the
kind level the same practices the Haskell community already considers to be good
at the type level.

Where-binds are quite common in idiomatic Haskell, and often contain non-trivial
logic. Their main use is in cases where we want to 'trap' certain arguments or
definitions from their parent function. They are also a common refactoring, as
well as a 'typed-hole-development' tool, where you create a hole to be filled
with a where-bound definition. In this kind of situation, having an explicit
signature on the where-bind helps: during development, this means you can use
typed holes within the where-bind to assist you, while without a signature, you
would get nothing. This is also advantageous in cases where we want to refactor
by 'promoting' where-binds to the top level, as the signature is already there
for us. While in theory, we should extend this to `let` bindings as well, they
are much rarer, and can be given signatures with `::` if `ScopedTypeVariables`
is on (which for us, it is by default) if needed. There's few reasons not to do
this, and many benefits, which is why we make it mandatory: the above examples
regarding Plutarch and constraints only make it more sensible.

While it is possible to provide definitions for multiple signatures at once at
the module level, it is almost never a good idea. Even in fairly straightforward
cases (like the example provided), it can be confusing, and in cases where the
'definition disassembly' is more complex (or involves other language features,
like named field puns or wildcards), it definitely _is_ confusing. Furthermore,
it's almost never warranted: it can be more concise, but only at the cost of
clarity, which is never a viable long-term tradeoff. Lastly, refactoring and
documenting such multi-definitions is more difficult. Keeping strictly to a 'one
signature, one definition' structure aids readability and maintainance, and is
almost never particularly verbose anyway.

### Other

Partial _Haskell_ functions MUST NOT be defined. Partial _Haskell_ functions
SHOULD NOT be used; the only exception is to ensure that another function is
total in a situation where the type system cannot prove it.

Partial _Plutarch_ functions MUST use `ptraceError` with an explanatory message
to fail. The message SHOULD contain the name of the function that errored, as
well as an explanation of the reason why.

Derivations MUST use an explicit
[strategy](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies).
Thus, the following is wrong:

```haskell
-- Do not do this!
newtype Foo (Bar Int)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
```

This is the correct form:

```haskell
newtype Foo = Foo (Bar Int)
  deriving stock (Generic, Show)
  deriving newtype (Eq)
  deriving anyclass (FromJSON, ToJSON)
```

Via-derivations SHOULD be preferred to newtype derivations, especially when the
underlying representation could change significantly. `Show` instances SHOULD be
stock derived. `Eq` and `Ord` instances SHOULD be via-derived or newtype derived
if possible.

`type` SHOULD NOT be used.

Sum types containing record fields MUST NOT be defined. Thus, the following is
not allowed:

```haskell
-- Do not do this!
data Foo = Bar | Baz { quux :: Int }
```

#### Justification

Partial functions are runtime bombs waiting to explode. The number of times the
'impossible' happened, especially in production code, is significant in our
experience, and most partiality is easily solvable. Allowing the compiler to
support our efforts, rather than being blind to them, helps us write clearer,
more robust and more informative code. Partiality is also an example of legacy,
and it is legacy of _considerable_ weight. While we do sometimes need an 'escape
hatch' due to being unable to explain what we want to the compiler, this should
be the _exception_, not the rule.

In Plutarch, partiality is tolerated, as requiring totality to a similar level
as Haskell isn't practical, due to on-chain space considerations. Furthermore, a
crashing script won't bring down our entire application. At the same time, we
want to ensure crashes are diagnosable, and the reasons for the crash are clear.
In particular, we want to know what specific function caused us to crash, to
help us debug and test in the future. 

Derivations are one of the most useful features of GHC, and extend the
capabilities of Haskell2010 considerably. However, with great power comes great
responsibility, especially with `GeneralizedNewtypeDeriving`. While there _is_
an unambiguous choice if no strategy is specified, this is hard to remember, and
is needless non-local information anyway. This is especially dire when
`GeneralizedNewtypeDeriving` combines with `DeriveAnyClass` on a newtype.
Explicit strategies not only state unambiguously and clearly what we want, and
also gives us more control over how the derivations are done. This reduces how
much we need to remember, as well as giving us precise control where needed.
Lastly, and not least of all, we can use via-derivations to remove considerable
amounts of boilerplate, but this _requires_ us to specify our strategy
explicitly. 

We recommend via-derivations over newtype derivations in general, as via
derivations are more explicit. If we use a newtype derivation, then change the
underlying type, as long as that type has the necessary instances, the code will
continue to compile with no warning. As types can potentially have quite
different behaviours for the same type class, it's better to be explicit and
have the compiler check our consistency.

`type` is a terrible idea in Haskell. It doesn't create an abstraction boundary,
as operations on the 'underlying type' still work over the `type` synonym.
Furthermore, it makes compiler output _very_ inconsistent where the synonym is
concerned: sometimes it will show the synonym, sometimes the underlying type. If
your goal is to create an abstraction boundary with its own operations,
`newtype` is both cost-free and clearer; if that is _not_ your goal, just use
the type directly, since it's semantically-equivalent and clearer. There is
absolutely no reason to use `type` to do anything.

The combination of record syntax and sum types, while allowed, [causes
considerable issues](https://stackoverflow.com/a/37657296/2629787). The main
problem with this problem is that it silently sneaks partiality in 'via the back
door', which, given our stance against partiality, is definitely not desirable.
While arguably useful in some cases, this extra trouble doesn't make it worth
its weight.

## Design practices

### Parse, don't validate

[Boolean blindness](http://dev.stephendiehl.com/hask/#boolean-blindness) SHOULD
NOT be used in the design of any function or API. Returning more meanginful data
SHOULD be the preferred choice. The general principle of ['parse, don't
validate'](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate)
should guide both design and implementation.

#### Justification

The [description of boolean
blindness](http://dev.stephendiehl.com/hask/#boolean-blindness) gives specific
reasons why it is a bad choice from a design and usability point of view. In
many cases, it is possible to give back a more meaningful response than 'yes' or
'no', and we should aim to do that. Designs that avoid boolean blindness are
more flexible, less bug-prone, and allow the type checker to assist us when
writing. This reduces cognitive load, improves our ability to refactor, and
means fewer bugs from things the compiler _could_ have checked had the function
_not_ been boolean-blind.

'Parse, don't validate' as a design philosophy can be thought of as an extension
of 'no boolean blindness'. Its
[description](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate)
specifies its benefits and explores this connection.

### No multi-parameter type classes without functional dependencies

Any multi-parameter type class MUST have a functional dependency restricting its
relation to a one-to-many at most. In cases of true many-to-many relationships,
type classes MUST NOT be used as a solution.

#### Justification

Single-parameter type classes can be seed as subsets of `Hask`; by this logic,
multi-parameter type classes describe _relations_ on `Hask`. While useful, and a
natural extension, multi-parameter type classes make type inference _extremely_
flakey, as the global coherence condition can often lead to the compiler being
unable to determine what instance you mean. This can happen even if all the type
parameters are concrete, as anyone can add a new instance at any time. This
comes directly from the assumption by the compiler that a multi-parameter type
class effectively represents an arbitrary many-to-many relation.

When we do _not_ have arbitrary many-to-many relations, multi-parameter type
classes are useful and convenient. We can indicate this using functional
dependencies, which inform the type checker that our relationship is not
arbitrarily many-to-many: more precisely, we specify that certain type variables
determine others, making the relation more restricted. This is standard practice
in many libraries (`mtl` being the most ubiquitous example), and allows us the
benefits of multi-parameter type classes without making type checking more
confusing and difficult.

In general, many-to-many relationships pose difficult design choices, for which
type classes are _not_ the correct solution. If we cannot provide a functional
dependency for a type class, it suggests that the design is either incomplete,
or relies inherently on a many-to-many relation. This means we must either
rethink the design to eliminate the many-to-many, or deal with it some other,
more appropriate, way.

### Type classes must have laws

Any type class that is not from an external dependency MUST have laws. These
laws MUST be documented in a Haddock comment on the type class definition, and
all instances MUST follow these laws.

#### Justification

Type classes are a powerful (and arguably, _defining_) feature of Haskell, but
can also be its most confusing. As they allow arbitrary ad-hoc polymorphism, and
are globally visible, we must limit the confusion this can produce.
Additionally, type classes without laws inhibit both people seeking to _use_ the
type class method, and also the people who want to _define_ instances of it. The
first group have no idea what to expect - they can't use equational reasoning,
one of Haskell's biggest strengths - in a setting where it's arguably at its
most necessary; the second group have no idea what their instance 'ought to do'.

Additionally, type classes with laws allow the construction of _provably_
correct abstractions on top of them. This is also a common feature of Haskell:
everything from monadic control structures to profunctor optics are evidence of
this. If we define our own type classes, we want to be able to abstract on top
of them with _total_ confidence that we are going to have correct results.
Lawless type classes make this difficult or outright impossible: consider the
number of abstractions built atop of `Monad`, as opposed to `IsString` or
`Foldable`. 

Therefore, by ensuring that all our type classes have laws, we make life easier
for both people using their instances, and also defining new instances. We gain
ease of understanding, additional flexibility, and greater abstractive power.

## Libraries and frameworks

### Use `Type.Reflection` instead of `Data.Typeable`

`Data.Typeable` from `base` SHOULD NOT be used; the only exception is for
interfacing with legacy libraries. Whenever its capabilities are required,
[`Type.Reflection`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Type-Reflection.html)
SHOULD be used instead.

#### Justification

`Data.Typeable` was the first attempt to bring runtime type information to GHC;
this mechanism is necessary, as GHC normally performs type erasure. The original
design of `Data.Typeable.Typeable` required the construction of a `TypeRep`,
which could be user-specified, representing the 'structure' of a type. This led
to issues of correctness, as a user-specified `TypeRep` could easily violate
assumptions, as well as coherency, given that for any given type, there was no
guarantee that its `TypeRep` would be unique. These problems later led to the
development of the `DeriveDataTypeable` extension, which made it impossible to
define `Data.Typeable.Typeable` except through the mechanisms provided by GHC.

Additionally, as `Data.Typeable` predates `TypeApplications`, its API requires a
value of a specific type to direct which `TypeRep` to provide. This suffers from
similar problems as `Data.Storable.sizeOf`, as there frequently isn't a sensible
value to provide. This forced developers to write code like

```haskell
typeOf (undefined :: a)
```

This looks strange, and isn't the approach taken by modern APIs. Lastly,
`Data.Typeable` had to be derived for any type that wanted to use its
mechanisms, which forced developers to 'pay' for these instances, whether they
wanted to or not, in case someone needed them.

`Type.Reflection` has been the go-to API for the purpose of runtime type
information since GHC 8.2. It improves the situation with `Data.Typeable` by
replacing the old mechanism with a compiler-generated singleton. Furthermore,
deriving `Typeable` is now unnecessary, for the same reason that deriving
`Coercible` is not necessary: GHC handles this for us. Additionally, the API is
now based on `TypeApplications`, which allows us to write

```haskell
typeRep @a
```

This system is also entirely pay-as-you-go. Instead of the responsibility being
placed on whoever _defines_ the data types (requiring paying the cost of the
instance whether you need it or not), the responsibility is now placed on the
_use_ sites: if you specify a `(Typeable a) =>` constraint, this informs GHC
that the singleton for `TypeRep a` is needed here, but not anywhere else.

Since `Type.Reflection` can do everything `Data.Typeable` can, has a more modern
API, and is also lower-cost, there is no reason to use `Data.Typeable` anymore
except for legacy compatibility reasons.

### Avoid GHC `Generic`

`GHC.Generics` SHOULD NOT be used as a means of deriving type class instances. It
MUST NOT be used for any other purpose.

#### Justification

`GHC.Generics` is an API that promises a lot, but delivers it only with many
problems. While it is arguably convenient for defining 'boilerplate' instances,
it is mostly convenient for whoever defines the _type class_, not the instances.
Furthermore, use of `GHC.Generics` has considerable costs:

* `Rep` has size quadratic in the size of the type being represented. This can
  cause enormous compile-time RAM costs: for example, records with 10+ fields
  can easily consume gigabytes of RAM at compile time, and generate huge
  binaries.
* The runtime performance of `GHC.Generics` defined methods can often be
  extremely bad, as `Rep`s often can't be inlined away due to the instances
  being driven by a type class defined in another library.
* Dealing with either of these problems requires egregious amounts of
  `INLINE`ing. This causes significant code blowup due to monomorphization,
  which leads to large compile times and larger binaries.
* Even with such amounts of `INLINE`ing, there are many cases where the inlining
  fails: for example, any method which 'traps' a higher-order constraint like
  `Functor`. These place you into two equally-awkward camps: either every
  instance ends up playing 'mother-may-I' with every 'trapped' type class
  method, leading to awful overheads; or you have to manually defunctionalize
  each type class using a free structure, by replacing `Functor` with `Yoneda`,
  for example. Both of these are obscure, difficult to detect, and _very_ hard
  to understand.
* The only way to use `GHC.Generics` to drive a derivation is to define a
  'private' type class, which is then instantiated for various `GHC.Generics`
  constructors. Then, the 'public' type class methods are implemented using
  `default` atop of the 'private' type class. This indirection is slow, inhibits
  cross-module optimizations, and leads to _very_ confusing error messages and
  documentation, except for users who are familiar with these specific caveats.
* Because of the above, you cannot combine `GHC.Generics`-driven defaults with
  default definitions at all.
* It is almost impossible to understand precisely what code gets run for an
  instance defined via defaulting to `GHC.Generics` unless you can read Core.

These problems are significant enough that GHC has recently had to [introduce a
new
flag](https://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/9.2.1-notes.html#compiler)
in an attempt to help optimize uses of `GHC.Generics` better. Therefore, any
convenience supposedly gained is offset by massive costs, which are hard to
diagnose, or even do much about. Thus, overall, we consider `GHC.Generics` to
not be worth their cost in general.
