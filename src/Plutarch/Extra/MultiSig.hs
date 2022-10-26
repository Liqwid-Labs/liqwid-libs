{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Plutarch.Extra.MultiSig
Maintainer : seungheon.ooh@gmail.com
Description: A basic N of M multisignature validation function.

A basic N of M multisignature validation function.
-}
module Plutarch.Extra.MultiSig (
  validatedByMultisig,
  pvalidatedByMultisig,
  PMultiSig (..),
  MultiSig,
  mkMultiSig,
  PMultiSigContext,
  pmultiSigContext,
) where

import GHC.Records (HasField)
import Optics.Getter (A_Getter, to)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Traversal (A_Traversal, traversalVL)
import Plutarch.Api.V2 (PPubKeyHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.Function (pflip)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import qualified PlutusTx (makeLift, unstableMakeIsData)

{- | A 'MultiSig' represents a proof that a particular set of signatures
     are present on a transaction.

     @since 3.8.0
-}
data MultiSig = MultiSig [PubKeyHash] Integer
  deriving stock
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

{- | Allows traversing over the list of 'PubKeyHash'es that must be present in
 the list of signatories.

 @since 3.8.0
-}
instance
  (k ~ A_Traversal, a ~ PubKeyHash, b ~ PubKeyHash) =>
  LabelOptic "keys" k MultiSig MultiSig a b
  where
  labelOptic = traversalVL $
    \f (MultiSig pkhs minSigs) -> MultiSig <$> traverse f pkhs <*> pure minSigs

{- | Allows access to (but not changing) the minimum number of signatories that
 must be present. Changing this field independently is forbidden, as it could
 construct an invalid result.

 @since 3.8.0
-}
instance
  (k ~ A_Getter, a ~ Integer, b ~ Integer) =>
  LabelOptic "minSigs" k MultiSig MultiSig a b
  where
  labelOptic = to $ \(MultiSig _ minSigs) -> minSigs

PlutusTx.makeLift ''MultiSig
PlutusTx.unstableMakeIsData ''MultiSig

{- | Given a list of keys, and a minimum number of signatories, returns
 'Nothing' if given too few keys, or 'Just' a 'MultiSig' otherwise.

 @since 3.8.0
-}
mkMultiSig :: [PubKeyHash] -> Integer -> Maybe MultiSig
mkMultiSig pkhs minSigs
  | length pkhs < fromIntegral minSigs = Nothing
  | otherwise = Just . MultiSig pkhs $ minSigs

{- | Plutarch-level MultiSig

     @since 0.1.0
-}
newtype PMultiSig (s :: S) = PMultiSig
  { getMultiSig ::
      Term
        s
        ( PDataRecord
            '[ "keys" ':= PBuiltinList (PAsData PPubKeyHash)
             , "minSigs" ':= PInteger
             ]
        )
  }
  deriving stock
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PDataFields
    )

-- | @since 1.4.0
instance DerivePlutusType PMultiSig where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PUnsafeLiftDecl PMultiSig where
  type PLifted PMultiSig = MultiSig

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData MultiSig PMultiSig)
  instance
    (PConstantDecl MultiSig)

-- | @since 3.2.0
instance PTryFrom PData PMultiSig

{- | Context required in order to check 'MultiSig'.

     Should be constructed with 'pmultiSigContext'.

     @since 3.8.0
-}
newtype PMultiSigContext (s :: S)
  = PMultiSigContext
      (Term s (PBuiltinList (PAsData PPubKeyHash)))
  deriving stock
    ( -- | @since 3.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.2.0
      PlutusType
    , -- | @since 3.2.0
      PEq
    )

-- | @since 3.2.0
instance DerivePlutusType PMultiSigContext where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.8.0
instance HasField "signatories" (PMultiSigContext s) (Term s (PBuiltinList (PAsData PPubKeyHash))) where
  getField (PMultiSigContext t) = t

--------------------------------------------------------------------------------

{- | Construct 'PMultiSigContext' providing the @signatories@ field,
     which typically comes from 'PTxInfo'.

     @since 3.2.0
-}
pmultiSigContext ::
  forall r (s :: S).
  ( HasField "signatories" r (Term s (PBuiltinList (PAsData PPubKeyHash)))
  ) =>
  r ->
  Term s PMultiSigContext
pmultiSigContext = pcon . PMultiSigContext . getField @"signatories"

{- | Check if a Haskell-level 'MultiSig' signs this transaction.

     @since 3.2.0
-}
validatedByMultisig :: forall (s :: S). MultiSig -> Term s (PMultiSigContext :--> PBool)
validatedByMultisig params =
  phoistAcyclic $
    pvalidatedByMultisig # pconstant params

{- | Check if a Plutarch-level MultiSig signs this transaction.

     @since 3.2.0
-}
pvalidatedByMultisig :: forall (s :: S). Term s (PMultiSig :--> PMultiSigContext :--> PBool)
pvalidatedByMultisig =
  phoistAcyclic $
    plam $ \multi ctx -> unTermCont $ do
      multiF <- pletAllC multi
      PMultiSigContext sigs <- pmatchC ctx

      pure $
        getField @"minSigs" multiF
          #<= ( plength #$ pfilter
                  # (pflip # pelem # sigs)
                  # getField @"keys" multiF
              )
