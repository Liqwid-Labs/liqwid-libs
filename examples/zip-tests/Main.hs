{-# LANGUAGE TypeApplications #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental

 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (cardinality, universeF), Universe (universe))
import Plutarch (
    S,
    Term,
    pcon,
    pfix,
    phoistAcyclic,
    plam,
    plet,
    pmatch,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (PBuiltinList, PBuiltinPair, pfstBuiltin, psndBuiltin)
import Plutarch.Integer (PInteger)
import Plutarch.List (
    PListLike (PElemConstraint),
    pcons,
    plength,
    pnil,
    puncons,
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.TermCont (tcont, unTermCont)
import Plutarch.Trace (ptraceError)

-- If you need QuickCheck stuff, import it from 'Test.QuickCheck', /not/
-- 'Test.Tasty.QuickCheck'! To see why, read [this
-- issue](https://github.com/UnkindPartition/tasty/issues/208)
import Test.QuickCheck (
    Gen,
    Property,
    arbitrary,
    getNonNegative,
    getPositive,
    shrink,
    vectorOf,
 )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutarch.Property (classifiedProperty)
import Test.Tasty.QuickCheck (testProperty)

-- For our example, we will define an \'enforcing zip\' function; instead of
-- truncating, this errors if the two lists being zipped don't match in size.
--
-- This is /very/ generically defined. You can consider @l1@, @l2@ and @l3@ to
-- all be 'PList' when reading this to get an idea of what it does.

pzipEnforcing ::
    forall
        (l3 :: (S -> Type) -> S -> Type)
        (a :: S -> Type)
        (b :: S -> Type)
        (c :: S -> Type)
        (l1 :: (S -> Type) -> S -> Type)
        (l2 :: (S -> Type) -> S -> Type)
        (s :: S).
    ( PElemConstraint l1 a
    , PListLike l1
    , PListLike l2
    , PElemConstraint l2 b
    , PListLike l3
    , PElemConstraint l3 c
    ) =>
    Term s ((a :--> b :--> c) :--> l1 a :--> l2 b :--> l3 c)
pzipEnforcing = phoistAcyclic $
    pfix #$ plam $ \self f txs tys -> unTermCont $ do
        tleft <- tcont (pmatch (puncons # txs))
        tright <- tcont (pmatch (puncons # tys))
        case (tleft, tright) of
            (PNothing, PNothing) -> pure pnil
            -- Always indicate errors with ptraceError and a sensible message, as
            -- otherwise, it is /very/ hard to track down why something blew up.
            (PNothing, _) -> pure . ptraceError $ "pzipEnforcing: first argument exhausted"
            (_, PNothing) -> pure . ptraceError $ "pzipEnforcing: second argument exhausted"
            (PJust t, PJust t') -> do
                PPair tx txs' <- tcont (pmatch t)
                PPair ty tys' <- tcont (pmatch t')
                pure $ pcons # (f # tx # ty) # (self # f # txs' # tys')

-- We want to verify the following property about 'pzipEnforcing': for any
-- 'PList's @xs@ and @ys@, if the length of @xs@ and the length of @ys@ are the
-- same, the length of @'pzipEnforcing' xs ys@ should also be the same;
-- otherwise, we should crash.
--
-- This is an example of a /conditional/ property: what we want to verify
-- depends on the input values. Furthermore, this property possesses a
-- \'crashing case\': specifically when the argument 'PList' lengths /don't/
-- match.
--
-- To verify this kind of property, we use 'classifiedProperty'.
--
-- First, we need a type to describe the groupings into which we want to divide
-- all of our inputs, which happen to be /pairs/ of 'PList's. In theory, we
-- could limit ourselves to two groupings (crashing and non-crashing); however,
-- this isn't as good as it could be. In reality, there are /two/ cases where we
-- could crash: either the first argument is too short, or the second argument
-- is too short. Treating these separately may identify more bugs, and thus, we
-- do this exactly.

data ZipCase = FirstTooShort | SecondTooShort | LengthsMatch
    deriving stock (Eq)

-- To work with 'classifiedProperty', we need to make 'ZipCase' a member of
-- three type classes:
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

instance Show ZipCase where
    show = \case
        FirstTooShort -> "first argument too short"
        SecondTooShort -> "second argument too short"
        LengthsMatch -> "equal lengths"

instance Universe ZipCase where
    universe = [FirstTooShort, SecondTooShort, LengthsMatch]

instance Finite ZipCase where
    universeF = universe
    cardinality = Tagged 3

-- We now need a \'classifier function\', which, given an input (in our case,
-- two lists), determines which of our three groups this case belongs to. It's
-- /essential/ that you get this right, as this is the most trusted part of what
-- you give to 'classifiedProperty'. We can use pattern matching to define this
-- in a fairly natural manner.

classifier :: forall (a :: Type). ([a], [a]) -> ZipCase
classifier = \case
    ([], []) -> LengthsMatch
    (_ : _, []) -> SecondTooShort
    ([], _ : _) -> FirstTooShort
    (_ : xs, _ : ys) -> classifier (xs, ys)

-- We now need a function which gives us a generator based on which group we are
-- in. This is slightly complicated by the fact that we are testing a
-- higher-kinded type: lists can contain basically anything we like. We resolve
-- this by choosing to use 'Integer'; if we wanted, we could define multiple
-- such functions, each producing different types.

generator :: ZipCase -> Gen ([Integer], [Integer])
generator = \case
    FirstTooShort -> do
        -- Generate the \'short length\'. We use a helper from QuickCheck to
        -- restrict possible values to be non-negative.
        shortLen <- getNonNegative <$> arbitrary
        -- Generate the \'extension\' of our short length. We again use a helper
        -- from QuickCheck to restrict possible values to be strictly positive.
        extension <- getPositive <$> arbitrary
        let longLen = shortLen + extension
        -- 'vectorOf' produces a list of /exactly/ requested length, given a
        -- generator for its elements.
        (,) <$> vectorOf shortLen arbitrary <*> vectorOf longLen arbitrary
    SecondTooShort -> do
        -- This is similar to the above. We could abstract over these, but for
        -- clarity, we just duplicate.
        shortLen <- getNonNegative <$> arbitrary
        extension <- getPositive <$> arbitrary
        let longLen = shortLen + extension
        (,) <$> vectorOf longLen arbitrary <*> vectorOf shortLen arbitrary
    LengthsMatch -> do
        -- Also similar to the above, but here, we only need one length.
        len <- getNonNegative <$> arbitrary
        (,) <$> vectorOf len arbitrary <*> vectorOf len arbitrary

-- We also need to provide a shrinker. You might be wondering why we have
-- group-specific /generators/, but not group-specific /shrinkers/; this is
-- because thanks to our  \'classifier function\', we can make sure that any
-- shrinks \'stay in their group\' automatically.
--
-- We use the 'Arbitrary' instance for lists to help us.

shrinker :: ([Integer], [Integer]) -> [([Integer], [Integer])]
shrinker (xs, ys) = (,) <$> shrink xs <*> shrink ys

-- Since our outcome will be different depending on which group our input is in,
-- we need a Plutarch-level function to compute the outcome. However, two of our
-- three cases are \'crashy\': instead of getting a result at all, we actually
-- expect our test to crash.
--
-- This is where we use the fact we return a 'PMaybe'. If our expected outcome
-- is 'PNothing', this means \'we expect this to crash, the value is
-- irrelevant\'; if it is a 'PJust', this means \'we expect this /not/ to crash,
-- and the result should be this\'.
--
-- Unfortunately, due to the way @plutarch-quickcheck@ is designed, we have to
-- use types that can be \'lifted\' into Plutarch from Haskell (and vice versa).
-- For Haskell-level lists, this is 'PBuiltinList', and for Haskell-level pairs,
-- this is 'PBuiltinPair'.

expected ::
    forall (s :: S).
    Term
        s
        ( PBuiltinPair (PBuiltinList PInteger) (PBuiltinList PInteger)
            :--> PMaybe PInteger
        )
expected = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        txs <- tcont (plet (pfstBuiltin # t))
        tys <- tcont (plet (psndBuiltin # t))
        tlenxs <- tcont (plet (plength # txs))
        tlenys <- tcont (plet (plength # tys))
        pure $ pif (tlenxs #== tlenys) (pcon . PJust $ tlenxs) (pcon PNothing)

-- This leaves us to specify the property itself. We take our inputs, use
-- 'pzipEnforcing', then return the result's length. Like before, we have to
-- decide on the \'zipping operator\', but since this doesn't matter in the
-- current case, we pick something simple.

definition ::
    forall (s :: S).
    Term
        s
        ( PBuiltinPair (PBuiltinList PInteger) (PBuiltinList PInteger)
            :--> PInteger
        )
definition = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        txs <- tcont (plet (pfstBuiltin # t))
        tys <- tcont (plet (psndBuiltin # t))
        tresult <- tcont (plet (pzipEnforcing @PBuiltinList # go # txs # tys))
        pure $ plength # tresult
  where
    -- This is kind of annoying, but is unfortunately necessary, since we can't
    -- simply \'re-use\' the @s@ we bound above, as we can't prove it's closed.
    go :: forall (s' :: S). Term s' (PInteger :--> PInteger :--> PInteger)
    go = phoistAcyclic $ plam $ \x y -> x + y

-- Now, we can \'plug in\' all of the pieces and get out a 'Property'.

ourProperty :: Property
ourProperty = classifiedProperty generator shrinker expected classifier definition

-- Now we can use @tasty-quickcheck@ to scaffold us up and run our tests.

main :: IO ()
main = do
    -- Always do this! It ensures you don't get encoding errors, and failed tests,
    -- for no reason.
    defaultMain . testGroup "Zip examples" $
        [ testProperty "Only works on equal lengths" ourProperty
        ]
