{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental

 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (DerivePNewtype (DerivePNewtype), PlutusType, S, Term)
import Plutarch.Builtin (PIsData)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (
    PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
    PUnsafeLiftDecl (PLifted),
 )
import Test.Tasty.Plutarch.Laws (
    Methodology (Methodology),
    pconstantDeclLaws,
 )

-- If you need QuickCheck stuff, import it from 'Test.QuickCheck', /not/
-- 'Test.Tasty.QuickCheck'! To see why, read [this
-- issue](https://github.com/UnkindPartition/tasty/issues/208)
import Test.QuickCheck (
    NonNegative (NonNegative),
    arbitrary,
    getNonNegative,
    shrink,
 )
import Test.Tasty (adjustOption, defaultMain)
import Test.Tasty.QuickCheck (QuickCheckTests)

-- For our example, we will define a type for natural numbers; that is, those
-- 'PInteger's which are not negative. There are several ways we could do this,
-- but we will use a newtype for simplicity.

newtype PNatural (s :: S) = PNatural (Term s PInteger)
    deriving (PlutusType, PIsData) via (DerivePNewtype PNatural PInteger)

-- We will also need a corresponding \'Haskell-level\' type. While we _could_
-- use the built-in 'Natural', we'll instead define another newtype to make it
-- clear what exactly we're doing.

newtype Natural = Natural Integer
    deriving stock (Show)
    deriving (Eq) via Integer

-- We assume that neither of these constructors will be exported, and that
-- suitable APIs are provided for both. We won't do this here, for reasons of
-- brevity.

-- First, we define an instance of 'PUnsafeLiftDecl'. This connects our
-- Plutarch-level type 'PNatural' with its Haskell-level type 'Natural'.

instance PUnsafeLiftDecl PNatural where
    type PLifted PNatural = Natural

-- Then, we need an instance 'PConstantDecl'. There are some consistencies that
-- are enforced by the type system; we note them where relevant.

instance PConstantDecl Natural where
    -- This is the (Haskell-level) type corresponding to 'Natural's on-chain
    -- representation.
    --
    -- This has to belong to the Plutus Core universe. In brief, this means it has
    -- to be 'Integer', 'ByteString' or 'Data', with possibly tuples or lists of
    -- these.
    type PConstantRepr Natural = Integer

    -- This is the /Plutarch/ type corresponding to 'Natural'. We must have:
    --
    -- PConstanted (PLifted t) = t
    type PConstanted Natural = PNatural

    -- The following methods describe how we can convert between the Haskell type
    -- and its representation; we allow this to be an /embedding/, which is
    -- necessary, since not every 'Integer' can be a 'Natural'.
    pconstantToRepr :: Natural -> Integer
    pconstantToRepr (Natural i) = i -- direct projection
    pconstantFromRepr :: Integer -> Maybe Natural
    pconstantFromRepr i
        | i >= 0 = pure . Natural $ i
        | otherwise = Nothing

-- We now want to verify that the laws for 'PConstantDecl' (and, by extension,
-- 'PUnsafeLiftDecl') are maintained. Briefly, those laws require that:
--
-- - 'pconstantToRepr' and 'pconstantFromRepr' form a partial isomorphism; and
-- - The claimed Plutarch type must be represented the same way as our stated
-- on-chain representation.
--
-- Since this consists of /multiple/ tests, the corresponding function produces
-- a 'TestTree' instead of a 'Property'.
--
-- In order to make such a 'TestTree', we have to provide two 'Methodology'
-- arguments, which correspond to combinations of generator and shrinker. One
-- 'Methodology' must be of the (Haskell-level) type; the other must be of its
-- (Haskell-equivalent) representation.
--
-- For 'Natural', we use the 'Arbitrary' instance of 'Integer' with a helper.

naturalMethodology :: Methodology Natural
naturalMethodology = Methodology (Natural . getNonNegative <$> arbitrary) go
  where
    go :: Natural -> [Natural]
    go (Natural i) = do
        i' <- getNonNegative <$> (shrink . NonNegative $ i)
        pure . Natural $ i'

-- For 'Integer', we simply re-use its 'Arbitrary' instance. For this
-- 'Methodology', we don't have to only generate \'correct\' representations;
-- it's fine for us to generate 'Integer's which can't be 'Natural's.

integerMethodology :: Methodology Integer
integerMethodology = Methodology arbitrary shrink

-- We have two choices: we can use 'pconstantDeclLaws' and
-- 'pconstantDeclLawsTrivial'. The only difference lies in how many values of
-- our \'representation type\' are convertable: if it's only a small fraction,
-- then 'pconstantDeclLaws' is the right choice; otherwise,
-- 'pconstantDeclLawsTrivial' is the right choice.
--
-- In this case, it's arguable that either one works: fortunately, we can try
-- both and see which one errors out more.

main :: IO ()
main = do
    -- Always do this! It ensures you don't get encoding errors, and failed tests,
    -- for no reason.
    setLocaleEncoding utf8
    -- Some of the law tests are universal properties, so we need to improve their
    -- test count to something useful. We use 'adjustOption' for this; it still
    -- allows people to set more tests on the command line if they choose to.
    defaultMain . adjustOption go . pconstantDeclLaws naturalMethodology $ integerMethodology
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
