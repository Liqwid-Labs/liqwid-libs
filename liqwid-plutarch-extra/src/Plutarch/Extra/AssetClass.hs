{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Provides Data and Scott encoded  asset class types and utility
  functions.
-}
module Plutarch.Extra.AssetClass (
  -- * AssetClass - Hask
  AssetClass (AssetClass, symbol, name),
  assetClassValue,

  -- * AssetClass - Plutarch
  PAssetClass (PAssetClass, psymbol, pname),
  passetClass,
  passetClassT,
  adaSymbolData,
  isAdaClass,
  adaClass,
  padaClass,
  emptyTokenNameData,
  psymbolAssetClass,
  psymbolAssetClassT,
  pconstantClass,
  pconstantClassT,

  -- * AssetClassData - Plutarch
  PAssetClassData (PAssetClassData),
  passetClassData,
  passetClassDataT,

  -- * Scott <-> Data conversions
  ptoScottEncoding,
  pfromScottEncoding,
  pviaScottEncoding,

  -- * Modifiers for QuickCheck
  AdaClassPresence (..),
  GenAssetClass (..),
) where

import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  ToJSONKey,
 )
import Data.Tagged (Tagged (Tagged, unTagged), untag)
import Generics.SOP qualified as SOP
import Optics.Getter (A_Getter, view)
import Optics.Label (LabelOptic, LabelOptic', labelOptic)
import Optics.Lens (A_Lens)
import Optics.Optic (Is, (%%))
import Optics.Setter (set)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Orphans ()
import Plutarch.Test.QuickCheck.Instances ()
import Plutarch.Test.QuickCheck.Modifiers (
  AdaSymbolPresence (WithAdaSymbol, WithoutAdaSymbol),
  GenCurrencySymbol (GenCurrencySymbol),
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx qualified
import Ply.Core.Class (
  PlyArg (
    UPLCRep,
    toBuiltinArg,
    toBuiltinArgData
  ),
 )
import Ply.Plutarch (PlyArgOf)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )

--------------------------------------------------------------------------------
-- AssetClass & Variants

{- | A version of 'PlutusTx.AssetClass', with a tag for currency.

 @since 3.10.0
-}
data AssetClass = AssetClass
  { symbol :: CurrencySymbol
  -- ^ @since 3.9.0
  , name :: TokenName
  -- ^ @since 3.9.0
  }
  deriving stock
    ( -- | @since 3.9.0
      Eq
    , -- | @since 3.9.0
      Ord
    , -- | @since 3.9.0
      Show
    , -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      ToJSON
    , -- | @since 3.9.0
      FromJSON
    , -- | @since 3.9.0
      FromJSONKey
    , -- | @since 3.9.0
      ToJSONKey
    , -- | @since 3.10.0
      SOP.Generic
    )
  deriving
    ( -- | @since 3.10.0
      PlutusTx.ToData
    , -- | @since 3.10.0
      PlutusTx.FromData
    , -- | @since 3.14.2
      PlutusTx.UnsafeFromData
    )
    via Plutarch.Extra.IsData.ProductIsData AssetClass
  deriving
    ( -- | @since 3.10.0
      Plutarch.Lift.PConstantDecl
    )
    via ( Plutarch.Extra.IsData.DerivePConstantViaDataList
            AssetClass
            PAssetClassData
        )

{- | A Scott-encoded Plutarch equivalent to 'AssetClass'.

 @since 3.10.0
-}
data PAssetClass (s :: S) = PAssetClass
  { psymbol :: Term s (PAsData PCurrencySymbol)
  -- ^ @since 3.9.0
  , pname :: Term s (PAsData PTokenName)
  -- ^ @since 3.9.0
  }
  deriving stock
    ( -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      PEq
    , -- | @since 3.9.0
      PlutusType
    )

-- | @since 3.10.0
instance DerivePlutusType PAssetClass where
  type DPTStrat _ = PlutusTypeScott

-- | @since 3.10.0
passetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
  plam $ \sym tk ->
    pcon $ PAssetClass (pdata sym) (pdata tk)

-- | @since 3.10.4
passetClassT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PTagged unit PAssetClass)
passetClassT = phoistAcyclic $
  plam $ \sym tk ->
    ppure #$ passetClass # sym # tk

-- | @since 3.10.0
pconstantClass ::
  forall (a :: Type) (k :: Type) (s :: S).
  ( Is k A_Getter
  , LabelOptic' "symbol" k a CurrencySymbol
  , LabelOptic' "name" k a TokenName
  ) =>
  a ->
  Term s PAssetClass
pconstantClass ac =
  pcon $
    PAssetClass
      (pconstantData $ view #symbol ac)
      (pconstantData $ view #name ac)

-- | @since 3.10.4
pconstantClassT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Tagged unit AssetClass ->
  Term s (PTagged unit PAssetClass)
pconstantClassT (Tagged (AssetClass sym tk)) =
  ppure #$ pcon $ PAssetClass (pconstantData sym) (pconstantData tk)

{- | Construct a 'PAssetClass' with empty 'pname'.
 @since 3.9.0
-}
psymbolAssetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PAssetClass)
psymbolAssetClass = phoistAcyclic $
  plam $ \sym ->
    pcon $ PAssetClass (pdata sym) emptyTokenNameData

{- | Tagged version of `psymbolAssetClass`
 @since 3.10.4
-}
psymbolAssetClassT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Term s (PCurrencySymbol :--> PTagged unit PAssetClass)
psymbolAssetClassT = phoistAcyclic $
  plam $ \sym ->
    ppure #$ pcon $ PAssetClass (pdata sym) emptyTokenNameData

--------------------------------------------------------------------------------
-- Simple Helpers

{- | Check whether an 'AssetClass' corresponds to Ada.

 @since 3.9.0
-}
isAdaClass ::
  forall (a :: Type) (k :: Type).
  ( Is k A_Getter
  , LabelOptic' "symbol" k a CurrencySymbol
  , LabelOptic' "name" k a TokenName
  ) =>
  a ->
  Bool
isAdaClass ac = view #symbol ac == s' && view #name ac == n'
  where
    (Tagged (AssetClass s' n')) = adaClass

{- | The 'PCurrencySymbol' for Ada, corresponding to an empty string.

 @since 3.9.0
-}
adaSymbolData :: forall (s :: S). Term s (PAsData PCurrencySymbol)
adaSymbolData = pconstantData ""

{- | The 'AssetClass' for Ada, corresponding to an empty currency symbol and
 token name.

 @since 3.9.0
-}
adaClass :: Tagged "Ada" AssetClass
adaClass = Tagged $ AssetClass "" ""

{- | Plutarch equivalent for 'adaClass'.

 @since 3.9.0
-}
padaClass :: forall (s :: S). Term s (PTagged "Ada" PAssetClass)
padaClass = pconstantClassT adaClass

{- | The empty 'PTokenName'

 @since 3.9.0
-}
emptyTokenNameData :: forall (s :: S). Term s (PAsData PTokenName)
emptyTokenNameData = pconstantData ""

----------------------------------------
-- Data-Encoded version

{- | A 'PlutusTx.Data'-encoded version of 'AssetClass', without the currency
 tag.

 @since 3.10.0
-}
newtype PAssetClassData (s :: S)
  = PAssetClassData
      ( Term
          s
          ( PDataRecord
              '[ "symbol" ':= PCurrencySymbol
               , "name" ':= PTokenName
               ]
          )
      )
  deriving stock
    ( -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      PlutusType
    , -- | @since 3.9.0
      PEq
    , -- | @since 3.9.0
      PIsData
    , -- | @since 3.9.0
      PDataFields
    , -- | @since 3.9.0
      PShow
    )

-- | @since 3.10.0
instance DerivePlutusType PAssetClassData where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.10.0
instance Plutarch.Lift.PUnsafeLiftDecl PAssetClassData where
  type PLifted PAssetClassData = AssetClass

-- | @since 3.21.3
instance PTryFrom PData (PAsData PAssetClassData)

-- | @since 3.10.0
passetClassData ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClassData)
passetClassData = phoistAcyclic $
  plam $ \sym tk ->
    mkRecordConstr
      PAssetClassData
      ( #symbol
          .= pdata sym
          .& #name
          .= pdata tk
      )

-- | @since 3.10.4
passetClassDataT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PTagged unit PAssetClassData)
passetClassDataT = phoistAcyclic $
  plam $ \sym tk ->
    ppure #$ passetClassData # sym # tk

{- | Convert from 'PAssetClassData' to 'PAssetClass'.

 @since 3.10.4
-}
ptoScottEncoding ::
  forall (s :: S).
  Term s (PAssetClassData :--> PAssetClass)
ptoScottEncoding = phoistAcyclic $
  plam $ \cls ->
    pletFields @["symbol", "name"] cls $
      \cls' ->
        pcon $
          PAssetClass
            (getField @"symbol" cls')
            (getField @"name" cls')

{- | Convert from 'PAssetClass' to 'PAssetClassData'.

 @since 3.10.4
-}
pfromScottEncoding ::
  forall (s :: S).
  Term s (PAssetClass :--> PAssetClassData)
pfromScottEncoding = phoistAcyclic $
  plam $ \cls -> pmatch cls $
    \(PAssetClass sym tk) ->
      mkRecordConstr
        PAssetClassData
        ( #symbol
            .= sym
            .& #name
            .= tk
        )

{- | Wrap a function using the Scott-encoded 'PAssetClass' to one using the
 'PlutusTx.Data'-encoded version.

 @since 3.10.4
-}
pviaScottEncoding ::
  forall (a :: PType).
  ClosedTerm (PAssetClass :--> a) ->
  ClosedTerm (PAssetClassData :--> a)
pviaScottEncoding fn = phoistAcyclic $
  plam $ \cls ->
    fn #$ ptoScottEncoding # cls

{- | Version of 'assetClassValue' for tagged 'AssetClass' and 'Tagged'.

 @since 3.10.4
-}
assetClassValue ::
  forall {k :: Type} (unit :: k).
  Tagged unit AssetClass ->
  Tagged unit Integer ->
  Value.Value
assetClassValue (Tagged (AssetClass sym tk)) q =
  Value.singleton sym tk $ untag q

----------------------------------------
-- Field Labels

-- | @since 3.9.0
makeFieldLabelsNoPrefix ''PAssetClass

-- | @since 3.9.0
makeFieldLabelsNoPrefix ''AssetClass

-- | @since 3.10.2
instance
  (k ~ A_Lens, a ~ CurrencySymbol, b ~ CurrencySymbol, tag ~ tag') =>
  LabelOptic "symbol" k (Tagged tag AssetClass) (Tagged tag' AssetClass) a b
  where
  labelOptic = #unTagged %% #symbol

-- | @since 3.10.2
instance
  (k ~ A_Lens, a ~ TokenName, b ~ TokenName, tag ~ tag') =>
  LabelOptic "name" k (Tagged tag AssetClass) (Tagged tag' AssetClass) a b
  where
  labelOptic = #unTagged %% #name

----------------------------------------
-- Ply instances

-- | @since 3.10.4
instance PlyArg AssetClass where
  type UPLCRep AssetClass = [PlutusTx.Data]
  toBuiltinArg ac =
    case PlutusTx.toData @AssetClass ac of
      PlutusTx.List x -> x
      _ -> error "unreachable"
  toBuiltinArgData = PlutusTx.toData

-- | @since 3.10.4
type instance PlyArgOf PAssetClassData = AssetClass

{- | Type-level marker to indicate whether a 'GenAssetClass' can have the ADA
 'AssetClass' inside it or not.

 @since 3.11.1
-}
data AdaClassPresence = WithAdaClass | WithoutAdaClass
  deriving stock
    ( -- | @since 3.10.5
      Eq
    , -- | @since 3.10.5
      Show
    , -- | @since 3.10.5
      Ord
    )

{- | A helper newtype for QuickCheck use with 'AssetClass'es. Has a type-level
 tag to indicate whether it could potentially contain the ADA 'AssetClass'. We
 provide instances of 'Arbitrary', 'CoArbitrary' and 'Function' around this
 newtype, intended to act on the 'AssetClass' inside it.

 The easiest way to use this newtype is by pattern matching:

 > forAll arbitrary $ \((GenAssetClass ac) :: GenAssetClass WithAdaClass) -> ...

 You can also \'re-wrap\' for shrinking:

 > shrink (GenAssetClass ac :: GenAssetClass WithAdaClass)

 = Note

 Due to limitations in QuickCheck itself, 'GenCurrencySymbol' with the
 'WithAdaSymbol' tag over-represents the ADA symbol. We inherit this behaviour
 on all instances of 'GenAssetClass' with th 'WithAdaClass' tag.

 @since 3.11.1
-}
newtype GenAssetClass (p :: AdaClassPresence) = GenAssetClass AssetClass
  deriving
    ( -- | @since 3.10.5
      Eq
    )
    via AssetClass
  deriving stock
    ( -- | @since 3.10.5
      Show
    )

{- | This instance shrinks only in the 'TokenName', as 'CurrencySymbol's do not
 shrink.

 = Note

 If this would generate the ADA 'AssetClass', its 'TokenName' will be empty.

 @since 3.11.1
-}
instance Arbitrary (GenAssetClass 'WithAdaClass) where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    GenAssetClass <$> do
      GenCurrencySymbol sym :: GenCurrencySymbol 'WithAdaSymbol <- arbitrary
      let Value.CurrencySymbol inner = sym
      tn <-
        if inner == ""
          then pure . Value.TokenName $ ""
          else arbitrary
      pure $
        AssetClass
          { symbol = sym
          , name = tn
          }
  {-# INLINEABLE shrink #-}
  shrink (GenAssetClass ac) =
    GenAssetClass <$> do
      tn' <- shrink . view #name $ ac
      pure . set #name tn' $ ac

{- | This instance shrinks only in the 'TokenName', as 'CurrencySymbol's do not
 shrink.

 @since 3.11.1
-}
instance Arbitrary (GenAssetClass 'WithoutAdaClass) where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    GenAssetClass <$> do
      GenCurrencySymbol sym :: GenCurrencySymbol 'WithoutAdaSymbol <- arbitrary
      tn <- arbitrary
      pure $
        AssetClass
          { symbol = sym
          , name = tn
          }
  {-# INLINEABLE shrink #-}
  shrink (GenAssetClass ac) =
    GenAssetClass <$> do
      tn' <- shrink . view #name $ ac
      pure . set #name tn' $ ac

-- | @since 3.11.1
instance CoArbitrary (GenAssetClass 'WithAdaClass) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (GenAssetClass ac) =
    let asGen :: GenCurrencySymbol 'WithAdaSymbol =
          GenCurrencySymbol . view #symbol $ ac
     in coarbitrary asGen . coarbitrary (view #name ac)

-- | @since 3.11.1
instance CoArbitrary (GenAssetClass 'WithoutAdaClass) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (GenAssetClass ac) =
    let asGen :: GenCurrencySymbol 'WithoutAdaSymbol =
          GenCurrencySymbol . view #symbol $ ac
     in coarbitrary asGen . coarbitrary (view #name ac)

-- | @since 3.11.1
instance Function (GenAssetClass 'WithAdaClass) where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        GenAssetClass 'WithAdaClass ->
        (GenCurrencySymbol 'WithAdaSymbol, TokenName)
      into (GenAssetClass ac) =
        (GenCurrencySymbol . view #symbol $ ac, view #name ac)
      outOf ::
        (GenCurrencySymbol 'WithAdaSymbol, TokenName) ->
        GenAssetClass 'WithAdaClass
      outOf (GenCurrencySymbol sym, tn) =
        GenAssetClass $
          AssetClass
            { symbol = sym
            , name = tn
            }

-- | @since 3.11.1
instance Function (GenAssetClass 'WithoutAdaClass) where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        GenAssetClass 'WithoutAdaClass ->
        (GenCurrencySymbol 'WithoutAdaSymbol, TokenName)
      into (GenAssetClass ac) =
        (GenCurrencySymbol . view #symbol $ ac, view #name ac)
      outOf ::
        (GenCurrencySymbol 'WithoutAdaSymbol, TokenName) ->
        GenAssetClass 'WithoutAdaClass
      outOf (GenCurrencySymbol sym, tn) =
        GenAssetClass $
          AssetClass
            { symbol = sym
            , name = tn
            }
