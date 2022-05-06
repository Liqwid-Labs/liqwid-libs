{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental

 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (cardinality, universeF), Universe (universe))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (S, Term, pcon, phoistAcyclic, plam, (#), type (:-->))
import Plutarch.Bool (PBool (PTrue), (#<=), (#==))
import Plutarch.Integer (PInteger, prem)
import Plutarch.Maybe (PMaybe (PJust))
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

-- If you need QuickCheck stuff, import it from 'Test.QuickCheck', /not/
-- 'Test.Tasty.QuickCheck'! To see why, read [this
-- issue](https://github.com/UnkindPartition/tasty/issues/208)
import Test.QuickCheck (Gen, Property, arbitrary, shrink)
import Test.Tasty.Plutarch.Property (classifiedProperty, peqProperty)

-- For our example, we will define and test a Plutarch-level squaring function.

psquare ::
    forall (s :: S).
    Term s (PInteger :--> PInteger)
psquare = phoistAcyclic $ plam $ \i -> i * i

-- We want to verify two properties about 'psquare':
--
-- 1. The square of any integer is non-negative; and
-- 2. For any integer @i@, if @i@ is even, then its square is also even;
-- otherwise, its square is odd.
--
-- Property 1 is an example of a /universal/ property: it applies to /any/
-- value in our domain. Property 2, on the other hand, is an example of a
-- /conditional/ property: what exactly we want to verify depends on the value
-- being verified.
--
-- In @plutarch-quickcheck@, we use 'peqProperty' to define universal
-- properties, and 'classifiedProperty' to define conditional properties.
--
-- Let's begin with Property 1. In order to define a universal property,
-- 'peqProperty' requires the following things from us:
--
-- 1. An expected outcome (as a Plutarch 'Term');
-- 2. A generator for the Haskell equivalent of our Plutarch type.
-- 3. A shrinker for the Haskell equivalent of our Plutarch type.
-- 4. A description of what to do to produce an outcome on the Plutarch level.
--
-- In our case, the expected outcome should be a 'PBool': our property is a
-- predicate. Specifically, we want the outcome to always be 'PTrue'.

p1Outcome :: forall (s :: S). Term s PBool
p1Outcome = pcon PTrue

-- As our function to test takes a 'PInteger' input, we need its Haskell-level
-- equivalent: more precisely, we need 'PLifted' 'PInteger', which happens to be
-- 'Integer'. We define generators and shrinkers using 'Integer' instead of
-- @'Term' s 'PInteger'@ for several reasons:
--
-- - We have the full power of Haskell to generate and shrink values;
-- - We don't run into problems of term closure and the type system, as this
-- requires impredicative instantiation; and
-- - It acts as an extra guard against our tests just being a redefinition of
-- what we're trying to test in the first place.
--
-- Thus, we need a @'Gen' 'Integer'@ and an @Integer -> [Integer]@ shrinker.
-- Fortunately for us, 'Integer' is an instance of 'Arbitrary', so we can obtain
-- both easily.

p1Generator :: Gen Integer
p1Generator = arbitrary

p1Shrinker :: Integer -> [Integer]
p1Shrinker = shrink

-- Lastly, we need to describe how exactly we take a 'PInteger' into a 'PBool'.
-- This is where our property definition basically comes in. We want to check
-- that, no matter what 'PInteger' we get, if we square it, we shouldn't get a
-- negative number.

p1Definition :: forall (s :: S). Term s (PInteger :--> PBool)
p1Definition = phoistAcyclic $ plam $ \i -> 0 #<= psquare # i

-- We can now assemble Property 1 using 'peqProperty'.

property1 :: Property
property1 = peqProperty p1Outcome p1Generator p1Shrinker p1Definition

-- For Property 2, we have to do a little bit more work. From its description,
-- we can see it divides all 'PInteger's into two groups: even and odd. Based on
-- what group any given value falls into, what we want to verify changes.
--
-- To do this, we first have to define a type representing the classification of
-- values into these two groups.

data Parity = IsOdd | IsEven
    deriving stock (Eq)

-- We also have to make it a member of three type classes:
--
-- - 'Show' (to allow us to display case distributions);
-- - 'Universe' (to prove we can enumerate its values); and
-- - 'Finite' (to prove it's not infinite, and how many values it has).
--
-- In theory, you can derive all three of these automatically: 'Show' can be
-- @stock@-derived, and 'Universe' and 'Finite' can be derived if your type has
-- an instance of 'Generic'. We will derive all of these manually here: if you
-- want to find out how to derive 'Universe' and 'Finite' automatically, check
-- the documentation of the
-- [@universe@](https://hackage.haskell.org/package/universe-1.2.2/docs/Data-Universe.html)
-- library.

instance Show Parity where
    show = \case
        IsOdd -> "odd integer"
        IsEven -> "even integer"

instance Universe Parity where
    universe = [IsOdd, IsEven]

instance Finite Parity where
    universeF = universe
    cardinality = Tagged 2

-- The most critical part comes next: defining a \'classifier function\', which,
-- given an input, determines which group it belongs to. It's /essential/ that
-- you get this right, as this is the most trusted part of what you give to
-- 'classifiedProperty'. Fortunately, in our case, this is straightforward.

p2Classifier :: Integer -> Parity
p2Classifier i
    | even i = IsEven
    | otherwise = IsOdd

-- Now, we need to provide a function which provides a generator based on which
-- group we are in. In this case, we can still use 'arbitrary', but we double
-- the result we get for even numbers, and double the result we get and add one
-- for odd numbers.

p2Generator :: Parity -> Gen Integer
p2Generator = \case
    IsOdd -> (+ 1) . (* 2) <$> arbitrary
    IsEven -> (* 2) <$> arbitrary

-- We also need to provide a shrinker. You might be wondering why we have
-- group-specific /generators/, but not group-specific /shrinkers/; this is
-- because thanks to our  \'classifier function\', we can make sure that any
-- shrinks \'stay in their group\' automatically. Once again, we can just borrow
-- the shrinker from the 'Arbitrary' instance for 'Integer'.

p2Shrinker :: Integer -> [Integer]
p2Shrinker = shrink

-- For universal properties, we provided a single expected outcome. However, in
-- our situation, the expected outcome will be different depending on which
-- group our input is in. Therefore, instead of providing a single outcome, we
-- provide a Plutarch-level function to compute what the outcome should be.
--
-- You might be wondering why this function returns a 'PMaybe', rather than just
-- a result. This is to allow \'crashing cases\'; situations where, given a
-- particular kind of input, we can't give a result, because what we're testing
-- will crash. In that case, the \'expected value\' is irrelevant, since we'll
-- never get one: we represent this using 'PNothing'. In our case, we have no
-- \'crashing cases\', so we give a 'PJust' every time.

p2Expected :: forall (s :: S). Term s (PInteger :--> PMaybe PBool)
p2Expected = phoistAcyclic $ plam $ \i -> pcon . PJust $ prem # i # 2 #== 0

-- This leaves us to specify the property itself, which is straightforward.

p2Definition :: forall (s :: S). Term s (PInteger :--> PBool)
p2Definition = phoistAcyclic $ plam $ \i -> prem # (psquare # i) # 2 #== 0

property2 :: Property
property2 = classifiedProperty p2Generator p2Shrinker p2Expected p2Classifier p2Definition

-- Now we can use @tasty-quickcheck@ to scaffold us up and run our tests.
--
-- It is
-- worth noting that we have a universal property: this means that we have to
-- adjust the number of tests that @tasty-quickcheck@ will perform, as the
-- default (100) is too low to be of any use. We will perform 10,000 tests
-- instead, but leave the option to add more at the command line if people
-- choose.
--
-- If you have no universal properties, you can skip the step above; conditional
-- properties use a statistical measure to ensure that enough tests are run,
-- without you having to lift a finger.

main :: IO ()
main = do
    -- Always do this! It ensures you don't get encoding errors, and failed tests,
    -- for no reason.
    setLocaleEncoding utf8
    -- adjustOption allows us to set a minimum test count, while not forbidding
    -- more if people want them.
    defaultMain . adjustOption go . testGroup "Square examples" $
        [ testProperty "Squares of numbers aren't negative" property1
        , testProperty "Squares of numbers maintain parity" property2
        ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000 -- 'QuickCheckTests' has a 'Num' instance for convenience
