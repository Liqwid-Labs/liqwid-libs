{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Extra.ExtendedAssetClass (
  -- * Types

  -- ** Haskell
  ExtendedAssetClass (..),

  -- ** Plutarch
  PExtendedAssetClass (..),

  -- * Constants

  -- ** Haskell
  extendedAdaClass,

  -- ** Plutarch
  pextendedAdaClass,

  -- * Functions

  -- ** Haskell
  isExtendedAdaClass,
  eqClasses,
  unsafeToAssetClass,

  -- ** Plutarch
  pisExtendedAdaClass,
  pextendedAssetClassValueOf,
  pextendedAssetClassValueOf',
  peqClasses,
  punsafeToAssetClass,
  punsafeToAssetClassData,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  (.:),
 )
import Data.Aeson.Encoding (pair)
import Data.Tagged (unTagged)
import Data.Text (Text, unpack)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.Getter (A_Getter, to, view)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Setter (set)
import Plutarch.Api.V1 (
  AmountGuarantees,
  KeyGuarantees,
  PCurrencySymbol,
  PValue,
 )
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Extra.AssetClass (
  AssetClass (AssetClass),
  PAssetClass (PAssetClass),
  PAssetClassData (PAssetClassData),
  adaClass,
  ptoScottEncoding,
 )
import Plutarch.Extra.Value (
  passetClassValueOf,
  passetClassValueOf',
  psymbolValueOf,
 )
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Data,
  TokenName,
  toData,
 )
import PlutusTx.IsData (makeIsDataIndexed)
import Ply.Core.Class (PlyArg (UPLCRep, toBuiltinArg, toBuiltinArgData))
import Ply.Plutarch.Class (PlyArgOf)

{- | An 'AssetClass' whose 'TokenName' may or may not be relevant.

 @since 3.14.2
-}
data ExtendedAssetClass
  = -- | 'TokenName' is irrelevant
    AnyToken CurrencySymbol
  | -- | 'TokenName' is relevant
    FixedToken AssetClass
  deriving stock
    ( -- | @since 3.14.2
      Generic
    , -- | @since 3.14.2
      Show
    , -- | @since 3.14.2
      Eq
    , -- | @since 3.14.2
      Ord
    )

-- | @since 3.14.2
makeIsDataIndexed
  ''ExtendedAssetClass
  [ ('AnyToken, 0)
  , ('FixedToken, 1)
  ]

-- | @since 3.14.5
instance PlyArg ExtendedAssetClass where
  type UPLCRep ExtendedAssetClass = Data
  toBuiltinArg = toData
  toBuiltinArgData = toData

-- | @since 3.14.2
deriving via
  (DerivePConstantViaData ExtendedAssetClass PExtendedAssetClass)
  instance
    PConstantDecl ExtendedAssetClass

-- | @since 3.14.2
instance ToJSON ExtendedAssetClass where
  {-# INLINEABLE toJSON #-}
  toJSON = \case
    AnyToken x ->
      object
        [ ("tag", toJSON @Text "anyToken")
        , ("symbol", toJSON x)
        ]
    FixedToken x ->
      object
        [ ("tag", toJSON @Text "fixedToken")
        , ("symbol", toJSON . view #symbol $ x)
        , ("name", toJSON . view #name $ x)
        ]
  {-# INLINEABLE toEncoding #-}
  toEncoding = \case
    AnyToken x ->
      pairs $
        pair "tag" (toEncoding @Text "anyToken")
          <> pair "symbol" (toEncoding x)
    FixedToken x ->
      pairs $
        pair "tag" (toEncoding @Text "fixedToken")
          <> pair "symbol" (toEncoding . view #symbol $ x)
          <> pair "name" (toEncoding . view #name $ x)

-- | @since 3.14.2
instance FromJSON ExtendedAssetClass where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "ExtendedAssetClass" $ \obj -> do
    tag :: Text <- obj .: "tag"
    cs <- obj .: "symbol"
    case tag of
      "anyToken" -> do
        pure . AnyToken $ cs
      "fixedToken" -> do
        tn <- obj .: "name"
        pure . FixedToken . AssetClass cs $ tn
      _ -> fail $ "Not a valid tag: " <> unpack tag

{- | We can always retrieve a 'CurrencySymbol', but depending on what we have,
 we may be unable to change it.

 @since 3.14.2
-}
instance
  (k ~ A_Getter, a ~ CurrencySymbol, b ~ CurrencySymbol) =>
  LabelOptic "symbol" k ExtendedAssetClass ExtendedAssetClass a b
  where
  labelOptic = to $ \case
    AnyToken x -> x
    FixedToken x -> view #symbol x

{- | We may not necessarily have a 'TokenName' that matters; if we do, then
 changing it is fine.

 @since 3.14.2
-}
instance
  (k ~ An_AffineTraversal, a ~ TokenName, b ~ TokenName) =>
  LabelOptic "name" k ExtendedAssetClass ExtendedAssetClass a b
  where
  labelOptic = atraversal out $ \eac tn -> case eac of
    AnyToken _ -> eac
    FixedToken x -> FixedToken . set #name tn $ x
    where
      out :: ExtendedAssetClass -> Either ExtendedAssetClass TokenName
      out = \case
        eac@(AnyToken _) -> Left eac
        FixedToken x -> Right . view #name $ x

{- | We can always \'pull out\' an 'AssetClass', by essentially \'forgetting\'
 the significance of our \'TokenName\'. In cases where it's not significant,
 we stub it with the empty name.

 @since 3.14.2
-}
instance
  (k ~ A_Getter, a ~ AssetClass, b ~ AssetClass) =>
  LabelOptic "assetClass" k ExtendedAssetClass ExtendedAssetClass a b
  where
  labelOptic = to $ \case
    AnyToken x -> AssetClass x ""
    FixedToken x -> x

{- | A 'FixedToken' wrapper around 'adaClass'. Provided mostly for convenience.

 @since 3.15.1
-}
extendedAdaClass :: ExtendedAssetClass
extendedAdaClass = FixedToken . unTagged $ adaClass

{- | Compare an 'ExtendedAssetClass' to an 'AssetClass'.

 @since 3.15.3
-}
eqClasses :: ExtendedAssetClass -> AssetClass -> Bool
eqClasses eac ac = case eac of
  AnyToken cs -> cs == view #symbol ac
  FixedToken ac' -> ac == ac'

{- | Convert to an 'AssetClass'. We do this by either \'unwrapping\' a
 'FixedToken', or \'stubbing in\' an empty 'TokenName' for an 'AnyToken'.

 = Note

 This is /not/ a safe conversion in general (hence its name). For example:

 > cls = AnyTokenType sym
 >
 > v = assetClassValueOf (unsafeToAssetClass cls)
 >                       (singleton x "" 1 <> singleton x "a" 1)

 Then @v@ would equal @1@, when it's supposed to be @2@.

 There are some legitimate uses for this conversion, which is why it exists,
 but it should be used with care.

 @since 3.15.3
-}
unsafeToAssetClass :: ExtendedAssetClass -> AssetClass
unsafeToAssetClass = \case
  AnyToken cs -> AssetClass cs ""
  FixedToken ac -> ac

{- | Plutarch equivalent to 'ExtendedAssetClass'.

 @since 3.14.2
-}
data PExtendedAssetClass (s :: S)
  = PAnyToken (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PFixedToken (Term s (PDataRecord '["_0" ':= PAssetClassData]))
  deriving stock
    ( -- | @since 3.14.2
      Generic
    )
  deriving anyclass
    ( -- | @since 3.14.2
      PEq
    , -- | @since 3.14.2
      PlutusType
    , -- | @since 3.14.2
      PShow
    , -- | @since 3.14.2
      PIsData
    )

-- | @since 3.14.2
instance DerivePlutusType PExtendedAssetClass where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.14.2
instance PUnsafeLiftDecl PExtendedAssetClass where
  type PLifted PExtendedAssetClass = ExtendedAssetClass

-- | @since 3.14.5
type instance PlyArgOf PExtendedAssetClass = ExtendedAssetClass

{- | As 'passetClassValueOf', but for 'PExtendedAssetClass'.

 @since 3.14.2
-}
pextendedAssetClassValueOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PExtendedAssetClass :--> PValue keys amounts :--> PInteger)
pextendedAssetClassValueOf = phoistAcyclic $ plam $ \eac value ->
  pmatch eac $ \case
    PAnyToken t -> psymbolValueOf # pfromData (pfield @"_0" # t) # value
    PFixedToken t ->
      let t' = ptoScottEncoding #$ pfield @"_0" # t
       in passetClassValueOf # t' # value

{- | As 'pextendedAssetClassValueOf', but using a Haskell-level
 'ExtendedAssetClass'.

 @since 3.14.6
-}
pextendedAssetClassValueOf' ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  ExtendedAssetClass ->
  Term s (PValue keys amounts :--> PInteger)
pextendedAssetClassValueOf' eac = phoistAcyclic $ plam $ \value ->
  case eac of
    AnyToken cs -> psymbolValueOf # pconstant cs # value
    FixedToken ac -> passetClassValueOf' ac # value

{- | Compare a 'PExtendedAssetClass' to a 'PAssetClass'.

 @since 3.14.2
-}
peqClasses ::
  forall (s :: S).
  Term s (PExtendedAssetClass :--> PAssetClassData :--> PBool)
peqClasses = phoistAcyclic $ plam $ \eac acd ->
  pmatch eac $ \case
    PAnyToken t ->
      pfromData (pfield @"_0" # t) #== pfromData (pfield @"symbol" # acd)
    PFixedToken t -> pfromData (pfield @"_0" # t) #== acd

{- | Convert to a 'PAssetClass'. We do this by either \'unwrapping\' a
 'PFixedToken', or \'stubbing in\' an empty 'PTokenName' for 'PAnyToken'.

 = Note

 This is /not/ a safe conversion in general (hence its name). For example:

 > cls = pcon $ PAnyTokenType sym
 >
 > v = passetClassValueOf # (punsafeToAssetClass cls) #
 >  pconstant (singleton x "" 1 <> singleton x "a" 1)

 Then @v@ would equal @1@, when it's supposed to be @2@.

 There are some legitimate uses for this conversion (specifically for creating
 'Value's), which is why it exists, but it should be used with care.

 @since 3.15.0
-}
punsafeToAssetClass ::
  forall (s :: S).
  Term s (PExtendedAssetClass :--> PAssetClass)
punsafeToAssetClass = phoistAcyclic $ plam $ \eac ->
  pmatch eac $ \case
    PAnyToken t ->
      pcon . PAssetClass (pfield @"_0" # t) . pdata . pconstant $ ""
    PFixedToken t -> ptoScottEncoding #$ pfield @"_0" # t

{- | Convert to a 'PAssetClassData'.

 = Note

 This is not a safe conversion in general, for the same reasons as
 'punsafeToAssetClass'. All caveats on the use of 'punsafeToAssetClass' also
 apply to this function.

 @since 3.15.0
-}
punsafeToAssetClassData ::
  forall (s :: S).
  Term s (PExtendedAssetClass :--> PAssetClassData)
punsafeToAssetClassData = phoistAcyclic $ plam $ \eac ->
  pmatch eac $ \case
    PAnyToken t ->
      pcon
        . PAssetClassData
        $ pdcons # (pfield @"_0" # t) #$ pdcons # (pdata . pconstant $ "") # pdnil
    PFixedToken t -> pfield @"_0" # t

{- | Plutarch equivalent to 'extendedAdaClass'.

 @since 3.15.1
-}
pextendedAdaClass :: forall (s :: S). Term s PExtendedAssetClass
pextendedAdaClass = pconstant extendedAdaClass

{- | Verify whether an 'ExtendedAssetClass' is the ADA extended asset class.

 @since 3.15.1
-}
isExtendedAdaClass :: ExtendedAssetClass -> Bool
isExtendedAdaClass = (extendedAdaClass ==)

{- | As 'isExtendedAdaClass', but for Plutarch.

 @since 3.15.1
-}
pisExtendedAdaClass ::
  forall (s :: S).
  Term s (PExtendedAssetClass :--> PBool)
pisExtendedAdaClass = phoistAcyclic $ plam $ \eac ->
  eac #== pextendedAdaClass
