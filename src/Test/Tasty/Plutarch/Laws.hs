{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Module: Test.Tasty.Plutarch.Laws
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Helpers for testing various Plutarch type class laws.
-}
module Test.Tasty.Plutarch.Laws (
    -- * Type
    Methodology (..),

    -- * Functions
    pconstantDeclLaws,
    pconstantDeclLawsTrivial,
) where

import Control.Applicative (empty)
import Data.Either (isLeft)
import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import Plutarch (S)
import Plutarch.Builtin (PIsData, pdata, pfromData)
import Plutarch.Lift (
    PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
    PUnsafeLiftDecl (PLifted),
    pconstant,
    plift,
 )
import Plutarch.Unsafe (punsafeCoerce)
import Test.QuickCheck (
    Gen,
    Property,
    checkCoverage,
    classify,
    counterexample,
    cover,
    forAllShrinkShow,
    oneof,
    property,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Helpers (ourStyle)
import Test.Tasty.QuickCheck (testProperties)
import Text.PrettyPrint (
    hang,
    renderStyle,
 )
import Text.Show.Pretty (ppDoc, ppShow)
import Type.Reflection (
    Typeable,
    tyConName,
    typeRep,
    typeRepTyCon,
 )

{- | Combination of generator and shrinker for a type.

 @since 1.0.0
-}
data Methodology (a :: Type) = Methodology (Gen a) (a -> [a])

{- | Given a generator and shrinker for both a type and its on-chain
 representation, ensure that the laws for 'PConstantDecl' hold.

 = Note on test counts

 Non-coverage tests (some of which will be produced by this function) default
 to 100 iterations only. This is /nothing/ like sufficient for most types: we
 encourage increasing this using @tasty-quickcheck@ options to at least
 several thousand.

 = Note on coverage

 Several laws take the form of \'if a condition holds, then another thing must
 be true\'; most notably, if the condition /doesn't/ hold, the outcome is
 either trivial or irrelevant. Testing this directly is complicated: if the
 condition is sufficiently rare, your tests may /pass/, but they do so not
 because you meet the law, but because you never saw any cases where it
 applies!

 This function is written to generate tests under the assumption that @b@s
 being convertable into @a@s is uncommon: namely, that given an output from
 the supplied generator of @b@s, the chance that 'pconstantFromRepr' on that
 output gives a 'Just' is quite small. This lets us avoid the problem
 described above using a clever technique. However, keep an eye on the
 percentage of unexpected conversions that the laws tests from this function
 report: if this is too high, there is a chance the tests will fail due to bad
 coverage.

 If your situation is such that @b@s being convertable into @a@s is either
 trivial (in that it's /always/ possible) or very likely, use
 'pconstantDeclLawsTrivial' instead.

 @since 1.0.0
-}
pconstantDeclLaws ::
    forall (a :: Type) (b :: Type) (p :: S -> Type) (p' :: S -> Type).
    ( Typeable a
    , Show a
    , PConstantDecl a
    , Eq a
    , PConstantRepr a ~ b
    , Eq b
    , Show b
    , PUnsafeLiftDecl p
    , PConstanted a ~ p
    , PLifted p ~ a
    , PUnsafeLiftDecl p'
    , PLifted p' ~ b
    , PIsData p'
    , PIsData p
    ) =>
    Methodology a ->
    Methodology b ->
    TestTree
pconstantDeclLaws (Methodology genA shrA) (Methodology genB shrB) =
    testProperties
        (pconstantDeclName @a)
        [ pconstantFromToLaw genA shrA
        , ("If pconstantFromRepr x = Just y, then pconstantToRepr y = x", prop2)
        , pconstantReprSameLaw genA shrA
        ]
  where
    prop2 :: Property
    prop2 = forAllShrinkShow hackedGen hackedShr hackedShow $ \bs ->
        let datum = case bs of
                Left x -> x
                Right x -> x
            conversion = pconstantFromRepr @a datum
         in checkCoverage
                . cover 50.0 (isJust conversion) "Precondition met"
                . classify (isJust conversion && isLeft bs) "Unexpected conversions"
                $ case (conversion, bs) of
                    (Nothing, Left _) -> property True
                    (Nothing, Right _) -> unexpectedConversionFailure datum
                    (Just res, _) -> sameAsExpected datum . pconstantToRepr $ res
    hackedGen :: Gen (Either b b)
    hackedGen = oneof [Left <$> genB, Right . pconstantToRepr <$> genA]
    hackedShr :: Either b b -> [Either b b]
    hackedShr = \case
        Left mayFail -> Left <$> shrB mayFail
        Right shouldWork -> case pconstantFromRepr @a shouldWork of
            Nothing -> empty
            Just x -> Right . pconstantToRepr <$> shrA x
    hackedShow :: Either b b -> String
    hackedShow =
        renderStyle ourStyle . \case
            Left mayFail -> hang "May convert (unlikely)" 4 (ppDoc mayFail)
            Right shouldSucceed -> hang "Must convert" 4 (ppDoc shouldSucceed)

{- | As 'pconstantDeclLaws', but works under the assumption that most, or all,
 @b@s can be converted successfully to @a@s.

 The caveat on test counts for 'pconstantDeclLaws' also applies to this
 function.

 = Note on coverage

 Similarly to 'pconstantDeclLaws', the issue with \'if condition then
 property\' laws holds for this function. However, /un/like
 'pconstantDeclLaws', we take the opposite approach here: we assume that @b@s
 being convertable into @a@ is either trivial (always works) or common. These
 tests run with this assumption in mind, but will fail if this is not the
 case. We also specify the percentage of failed conversion from @b@ to @a@ as
 part of our output, to ensure that your assumptions hold.

 If @b@s only seldom convert into @a@s successfully, use 'pconstantDeclLaws'
 instead.

 @since 1.0.0
-}
pconstantDeclLawsTrivial ::
    forall (a :: Type) (b :: Type) (p :: S -> Type) (p' :: S -> Type).
    ( Typeable a
    , Show a
    , PConstantDecl a
    , Eq a
    , PConstantRepr a ~ b
    , Eq b
    , Show b
    , PUnsafeLiftDecl p
    , PConstanted a ~ p
    , PLifted p ~ a
    , PUnsafeLiftDecl p'
    , PLifted p' ~ b
    , PIsData p'
    , PIsData p
    ) =>
    Methodology a ->
    Methodology b ->
    TestTree
pconstantDeclLawsTrivial (Methodology genA shrA) (Methodology genB shrB) =
    testProperties
        (pconstantDeclName @a)
        [ pconstantFromToLaw genA shrA
        , ("If pconstantFromRepr x = Just y, then pconstantToRepr y = x", prop2)
        , pconstantReprSameLaw genA shrA
        ]
  where
    prop2 :: Property
    prop2 = forAllShrinkShow genB shrB ppShow $ \x ->
        let conversion = pconstantFromRepr @a x
         in checkCoverage
                . cover 50.0 (isJust conversion) "Precondition met"
                . classify (isNothing conversion) "Could not convert"
                $ case conversion of
                    Nothing -> property True
                    Just res -> sameAsExpected x . pconstantToRepr $ res

-- Individual names

pconstantDeclName :: forall (a :: Type). (Typeable a) => String
pconstantDeclName = "PConstantDecl for " <> typeName @a

-- Individual laws

pconstantFromToLaw ::
    forall (a :: Type).
    (Eq a, PConstantDecl a, Show a) =>
    Gen a ->
    (a -> [a]) ->
    (String, Property)
pconstantFromToLaw gen shr = (name, law)
  where
    name :: String
    name = "pconstantFromRepr . pconstantToRepr = Just"
    law :: Property
    law = forAllShrinkShow gen shr ppShow $ \x ->
        let expected = Just x
            actual = pconstantFromRepr . pconstantToRepr $ x
         in sameAsExpected expected actual

pconstantReprSameLaw ::
    forall (a :: Type) (b :: Type) (p :: S -> Type) (p' :: S -> Type).
    ( Show a
    , PConstantDecl a
    , PConstantRepr a ~ b
    , Eq b
    , Show b
    , PUnsafeLiftDecl p
    , PConstanted a ~ p
    , PLifted p ~ a
    , PUnsafeLiftDecl p'
    , PLifted p' ~ b
    , PIsData p'
    , PIsData p
    ) =>
    Gen a ->
    (a -> [a]) ->
    (String, Property)
pconstantReprSameLaw gen shr = (name, law)
  where
    name :: String
    name = "pconstantToRepr = plift . pfromData. punsafeCoerce . pdata . pconstant"
    law :: Property
    law = forAllShrinkShow gen shr ppShow $ \x ->
        let expected = pconstantToRepr x
            actual = plift (pfromData . punsafeCoerce . pdata . pconstant $ x)
         in sameAsExpected expected actual

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

sameAsExpected ::
    forall (a :: Type).
    (Eq a, Show a) =>
    a ->
    a ->
    Property
sameAsExpected expected actual = counterexample go . property $ expected == actual
  where
    go :: String
    go =
        renderStyle ourStyle $
            "Outcome did not match expected value.\n"
                <> hang "Expected" 4 (ppDoc expected)
                <> "\n"
                <> hang "Actual" 4 (ppDoc actual)

unexpectedConversionFailure :: forall (a :: Type). (Show a) => a -> Property
unexpectedConversionFailure x = counterexample go . property $ False
  where
    go :: String
    go =
        renderStyle ourStyle $
            "A value that should convert failed to do so.\n"
                <> hang "Value" 4 (ppDoc x)
