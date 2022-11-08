module Plutarch.Extra.ExtendedAssetClass (
  -- * Types

  -- ** Haskell
  AnyTokenAssetClass (..),
  FixedTokenAssetClass (..),
  ExtendedAssetClass (..),

  -- ** Plutarch
  PAnyTokenAssetClass (..),
  PFixedTokenAssetClass (..),
  PExtendedAssetClass (..),

  -- * Functions

  -- ** Plutarch
  pextendedAssetClassValueOf,
  peqClasses,
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
import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.Getter (A_Getter, to, view)
import Optics.Iso (An_Iso, coercedTo)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Lens (A_Lens)
import Optics.Optic ((%))
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
  PAssetClassData,
  ptoScottEncoding,
 )
import Plutarch.Extra.Value (passetClassValueOf, psymbolValueOf)
import Plutarch.Lift (
  PConstantDecl (
    PConstantRepr,
    PConstanted,
    pconstantFromRepr,
    pconstantToRepr
  ),
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Data (Constr),
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  TokenName,
  UnsafeFromData (unsafeFromBuiltinData),
  builtinDataToData,
  dataToBuiltinData,
  fromData,
  toData,
 )

{- | An 'AssetClass' whose 'TokenName' is irrelevant.

 @since 3.14.2
-}
newtype AnyTokenAssetClass = AnyTokenAssetClass CurrencySymbol
  deriving stock
    ( -- | @since 3.14.2
      Generic
    , -- | @since 3.14.2
      Show
    )
  deriving
    ( -- | @since 3.14.2
      Eq
    , -- | @since 3.14.2
      Ord
    )
    via CurrencySymbol

-- | @since 3.14.2
instance ToJSON AnyTokenAssetClass where
  {-# INLINEABLE toJSON #-}
  toJSON atac = object [("symbol", toJSON . view #symbol $ atac)]
  {-# INLINEABLE toEncoding #-}
  toEncoding = pairs . pair "symbol" . toEncoding . view #symbol

-- | @since 3.14.2
instance FromJSON AnyTokenAssetClass where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "AnyTokenAssetClass" $ \obj ->
    AnyTokenAssetClass <$> obj .: "symbol"

-- | @since 3.14.2
instance
  (k ~ A_Getter, a ~ CurrencySymbol, b ~ CurrencySymbol) =>
  LabelOptic "symbol" k AnyTokenAssetClass AnyTokenAssetClass a b
  where
  labelOptic = to coerce

{- | We can always \'pull out\' an 'AssetClass' by \'filling in\' an empty
 'TokenName'.

 @since 3.14.2
-}
instance
  (k ~ A_Getter, a ~ AssetClass, b ~ AssetClass) =>
  LabelOptic "assetClass" k AnyTokenAssetClass AnyTokenAssetClass a b
  where
  labelOptic = to $ \(AnyTokenAssetClass cs) -> AssetClass cs ""

-- | @since 3.14.2
instance PConstantDecl AnyTokenAssetClass where
  type PConstanted AnyTokenAssetClass = PAnyTokenAssetClass
  type PConstantRepr AnyTokenAssetClass = Data
  pconstantFromRepr = fromData
  pconstantToRepr = toData

-- | @since 3.14.2
instance FromData AnyTokenAssetClass where
  fromBuiltinData dat =
    AnyTokenAssetClass <$> case builtinDataToData dat of
      Constr 0 [dat'] -> fromData dat'
      _ -> Nothing

-- | @since 3.14.2
instance UnsafeFromData AnyTokenAssetClass where
  unsafeFromBuiltinData dat = case builtinDataToData dat of
    Constr 0 [dat'] -> case fromData dat' of
      Just cs -> AnyTokenAssetClass cs
      Nothing ->
        error "unsafeFromBuiltinData: Did not get a CurrencySymbol for AnyTokenAssetClass"
    _ -> error "unsafeFromBuiltinData: Could not make AnyTokenAssetClass"

-- | @since 3.14.2
instance ToData AnyTokenAssetClass where
  toBuiltinData (AnyTokenAssetClass cs) =
    dataToBuiltinData $ Constr 0 [toData cs]

{- | Plutarch equivalent to 'AnyTokenAssetClass'.

 @since 3.14.2
-}
newtype PAnyTokenAssetClass (s :: S)
  = PAnyTokenAssetClass (Term s PCurrencySymbol)
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
instance DerivePlutusType PAnyTokenAssetClass where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.14.2
instance PUnsafeLiftDecl PAnyTokenAssetClass where
  type PLifted PAnyTokenAssetClass = AnyTokenAssetClass

{- | An 'AssetClass' whose 'TokenName' is significant somehow.

 @since 3.14.2
-}
newtype FixedTokenAssetClass = FixedTokenAssetClass AssetClass
  deriving stock
    ( -- | @since 3.14.2
      Generic
    , -- | @since 3.14.2
      Show
    )
  deriving
    ( -- | @since 3.14.2
      Eq
    , -- | @since 3.14.2
      Ord
    )
    via AssetClass

-- | @since 3.14.2
instance PConstantDecl FixedTokenAssetClass where
  type PConstantRepr FixedTokenAssetClass = Data
  type PConstanted FixedTokenAssetClass = PFixedTokenAssetClass
  pconstantFromRepr = fromData
  pconstantToRepr = toData

-- | @since 3.14.2
instance FromData FixedTokenAssetClass where
  fromBuiltinData dat =
    FixedTokenAssetClass <$> case builtinDataToData dat of
      Constr 0 [dat'] -> fromData dat'
      _ -> Nothing

-- | @since 3.14.2
instance UnsafeFromData FixedTokenAssetClass where
  unsafeFromBuiltinData dat = case builtinDataToData dat of
    Constr 0 [dat'] -> case fromData dat' of
      Just ac -> FixedTokenAssetClass ac
      Nothing ->
        error "unsafeFromBuitinData: Did not get an AssetClass for FixedTokenAssetClass"
    _ -> error "unsafeFromBuiltinData: Could not make FixedTokenAssetClass"

-- | @since 3.14.2
instance ToData FixedTokenAssetClass where
  toBuiltinData (FixedTokenAssetClass ac) =
    dataToBuiltinData $ Constr 0 [toData ac]

-- | @since 3.14.2
instance
  (k ~ A_Lens, a ~ CurrencySymbol, b ~ CurrencySymbol) =>
  LabelOptic "symbol" k FixedTokenAssetClass FixedTokenAssetClass a b
  where
  labelOptic = coercedTo @AssetClass % #symbol

-- | @since 3.14.2
instance
  (k ~ A_Lens, a ~ TokenName, b ~ TokenName) =>
  LabelOptic "name" k FixedTokenAssetClass FixedTokenAssetClass a b
  where
  labelOptic = coercedTo @AssetClass % #name

-- | @since 3.14.2
instance ToJSON FixedTokenAssetClass where
  {-# INLINEABLE toJSON #-}
  toJSON ftac =
    object
      [ ("symbol", toJSON . view #symbol $ ftac)
      , ("name", toJSON . view #name $ ftac)
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ftac =
    pairs $
      pair "symbol" (toEncoding . view #symbol $ ftac)
        <> pair "name" (toEncoding . view #name $ ftac)

-- | @since 3.14.1
instance FromJSON FixedTokenAssetClass where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "FixedTokenAssetClass" $ \obj -> do
    cs <- obj .: "symbol"
    tn <- obj .: "name"
    pure . FixedTokenAssetClass . AssetClass cs $ tn

{- | 'FixedTokenAssetClass' and 'AssetClass' are isomorphic.

 @since 3.14.2
-}
instance
  (k ~ An_Iso, a ~ AssetClass, b ~ AssetClass) =>
  LabelOptic "assetClass" k FixedTokenAssetClass FixedTokenAssetClass a b
  where
  labelOptic = coercedTo @AssetClass

{- | Plutarch equivalent to 'FixedTokenAssetClass'.

 @since 3.14.2
-}
newtype PFixedTokenAssetClass (s :: S)
  = PFixedTokenAssetClass (Term s PAssetClassData)
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
instance DerivePlutusType PFixedTokenAssetClass where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.14.2
instance PUnsafeLiftDecl PFixedTokenAssetClass where
  type PLifted PFixedTokenAssetClass = FixedTokenAssetClass

{- | An 'AssetClass' whose 'TokenName' may or may not be relevant.

 @since 3.14.2
-}
data ExtendedAssetClass
  = AnyToken AnyTokenAssetClass
  | FixedToken FixedTokenAssetClass
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
instance FromData ExtendedAssetClass where
  fromBuiltinData dat = case builtinDataToData dat of
    Constr 0 [dat'] -> AnyToken <$> fromData dat'
    Constr 1 [dat'] -> FixedToken <$> fromData dat'
    _ -> Nothing

-- | @since 3.14.2
instance UnsafeFromData ExtendedAssetClass where
  unsafeFromBuiltinData dat = case fromBuiltinData dat of
    Just eac -> eac
    Nothing -> error "unsafeFromBuiltinData: Could not make an ExtendedAssetClass"

-- | @since 3.14.2
instance ToData ExtendedAssetClass where
  toBuiltinData =
    dataToBuiltinData . \case
      AnyToken x -> Constr 0 [toData x]
      FixedToken x -> Constr 1 [toData x]

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
        , ("symbol", toJSON . view #symbol $ x)
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
          <> pair "symbol" (toEncoding . view #symbol $ x)
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
        pure . AnyToken . AnyTokenAssetClass $ cs
      "fixedToken" -> do
        tn <- obj .: "name"
        pure . FixedToken . FixedTokenAssetClass . AssetClass cs $ tn
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
    AnyToken x -> view #symbol x
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
    AnyToken x -> view #assetClass x
    FixedToken x -> view #assetClass x

{- | Plutarch equivalent to 'ExtendedAssetClass'.

 @since 3.14.2
-}
data PExtendedAssetClass (s :: S)
  = PAnyToken (Term s (PDataRecord '["_0" ':= PAnyTokenAssetClass]))
  | PFixedToken (Term s (PDataRecord '["_0" ':= PFixedTokenAssetClass]))
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

{- | As 'passetClassValueOf', but for 'PExtendedAssetClass'.

 @since 3.14.2
-}
pextendedAssetClassValueOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PExtendedAssetClass :--> PValue keys amounts :--> PInteger)
pextendedAssetClassValueOf = phoistAcyclic $ plam $ \eac value ->
  pmatch eac $ \case
    PAnyToken t -> pmatch (pfromData $ pfield @"_0" # t) $ \(PAnyTokenAssetClass t') ->
      psymbolValueOf # t' # value
    PFixedToken t -> pmatch (pfromData $ pfield @"_0" # t) $ \(PFixedTokenAssetClass t') ->
      passetClassValueOf # (ptoScottEncoding # t') # value

{- | Compare a 'PExtendedAssetClass' to a 'PAssetClass'.

 @since 3.14.2
-}
peqClasses ::
  forall (s :: S).
  Term s (PExtendedAssetClass :--> PAssetClassData :--> PBool)
peqClasses = phoistAcyclic $ plam $ \eac acd ->
  pmatch eac $ \case
    PAnyToken t -> pmatch (pfromData $ pfield @"_0" # t) $ \(PAnyTokenAssetClass t') ->
      t' #== pfromData (pfield @"symbol" # acd)
    PFixedToken t -> pmatch (pfromData $ pfield @"_0" # t) $ \(PFixedTokenAssetClass t') ->
      t' #== acd
