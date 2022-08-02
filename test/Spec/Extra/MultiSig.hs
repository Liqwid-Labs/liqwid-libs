{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module     : Spec.Extra.MultiSig
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'MultiSig' functions

Property model and tests for 'MultiSig' functions
-}
module Spec.Extra.MultiSig (tests) where

import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Data.ByteString.Hash (sha2_256)
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (..), Universe (..))
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Context
import Plutarch.Extra.MultiSig (
    MultiSig (MultiSig),
    PMultiSig,
    pvalidatedByMultisig,
 )
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Prelude
import PlutusLedgerApi.V1 (
    PubKeyHash (..),
    ScriptContext (..),
    ScriptPurpose (..),
    TxInfo (txInfoSignatories),
    TxOutRef (..),
    toBuiltin,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative)
import Test.Tasty.QuickCheck (
    Gen,
    Property,
    chooseAny,
    chooseInt,
    listOf,
    testProperty,
    vectorOf,
 )

-- | Model for testing multisigs.
type MultiSigModel = (MultiSig, ScriptContext)

-- | Propositions that may hold true of a `MultiSigModel`.
data MultiSigProp
    = -- | Sufficient number of signatories in the script context.
      MeetsMinSigs
    | -- | Insufficient number of signatories in the script context.
      DoesNotMeetMinSigs
    deriving stock (Eq, Show, Ord)

instance Universe MultiSigProp where
    universe = [MeetsMinSigs, DoesNotMeetMinSigs]

instance Finite MultiSigProp where
    universeF = universe
    cardinality = Tagged 2

genHashByteString :: Gen C.ByteString
genHashByteString = sha2_256 . C.pack . show <$> (chooseAny :: Gen Integer)

genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = PubKeyHash . toBuiltin <$> genHashByteString

-- | Generate model with given proposition.
genMultiSigProp :: MultiSigProp -> Gen MultiSigModel
genMultiSigProp p = do
    size <- chooseInt (4, 20)
    pkhs <- vectorOf size genPubKeyHash
    minSig <- chooseInt (1, length pkhs)
    othersigners <- take 20 <$> listOf genPubKeyHash

    let ms = MultiSig pkhs (toInteger minSig)

    n <- case p of
        MeetsMinSigs -> chooseInt (minSig, length pkhs)
        DoesNotMeetMinSigs -> chooseInt (0, minSig - 1)

    let builder :: (Monoid a, Builder a) => a
        builder = mconcat $ signedWith <$> take n pkhs <> othersigners
        txinfo = buildTxInfoUnsafe builder
    pure (ms, ScriptContext txinfo (Spending (TxOutRef "" 0)))

-- | Classify model into propositions.
classifyMultiSigProp :: MultiSigModel -> MultiSigProp
classifyMultiSigProp (MultiSig keys (fromIntegral -> minsig), ctx)
    | minsig <= length signer = MeetsMinSigs
    | otherwise = DoesNotMeetMinSigs
  where
    signer = filter (`elem` keys) $ txInfoSignatories . scriptContextTxInfo $ ctx

-- | Shrinker. Not used.
shrinkMultiSigProp :: MultiSigModel -> [MultiSigModel]
shrinkMultiSigProp = const []

-- | Expected behavior of @pvalidatedByMultisig@.
expectedHs :: MultiSigModel -> Maybe Bool
expectedHs model = case classifyMultiSigProp model of
    MeetsMinSigs -> Just True
    _ -> Just False

-- | Actual implementation of @pvalidatedByMultisig@.
actual :: Term s (PBuiltinPair PMultiSig PScriptContext :--> PBool)
actual = plam $ \x -> unTermCont $ do
    ms <- pletC $ pfstBuiltin # x
    sc <- pletC $ psndBuiltin # x
    pure $ pvalidatedByMultisig # ms # (pfield @"txInfo" # sc)

-- | Proposed property.
prop :: Property
prop = classifiedPropertyNative genMultiSigProp shrinkMultiSigProp expectedHs classifyMultiSigProp actual

tests :: [TestTree]
tests =
    [ testProperty "MultiSig property" prop
    ]
