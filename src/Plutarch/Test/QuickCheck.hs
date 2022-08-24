{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck (
    type IsPLam,
    type IsLast,
    type PTT,
    type TestableFun,
    type PLamArgs,
    type PA,
    type PB,
    type PC,
    FromPFunN (..),
    fromPFun,
    haskEquiv',
    haskEquiv,
    shrinkPLift,
    arbitraryPLift,
    PFun (..),
    pattern PFn,
    TestableTerm (..),
    PArbitrary (..),
    pconstantT,
    pliftT,
    uplcEq,
) where

import Data.Kind (Type)
import Generics.SOP (All, HPure (hcpure), NP ((:*)), Proxy (Proxy))
import Plutarch (Config (Config), TracingMode (NoTracing), compile, tracingMode)
import Plutarch.Evaluate (evalScript)
import Plutarch.Lift (DerivePConstantViaNewtype (..), PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Num (PNum)
import Plutarch.Prelude (
    DPTStrat,
    DerivePlutusType,
    Generic,
    PBool,
    PEq,
    PInteger,
    PIsData,
    PLift,
    POrd,
    PPartialOrd,
    PlutusType,
    PlutusTypeNewtype,
    S,
    Term,
    pcon,
    plift,
    pto,
    (#),
    type (:-->),
 )
import Plutarch.Show (PShow)
import Plutarch.Test.QuickCheck.Function (
    PFun (..),
    pattern PFn,
 )
import Plutarch.Test.QuickCheck.Instances (
    PArbitrary (..),
    PCoArbitrary (..),
    TestableTerm (..),
    pconstantT,
    pliftT,
 )
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    Testable (property),
    forAll,
 )

-- | @since 2.0.0
data PTermType
    = LastPFunction
    | LastPTerm
    | PFunction
    | PTerm

-- | @since 2.0.0
type family PTT' (isplam :: Bool) (end :: Bool) :: PTermType where
    PTT' 'True 'True = 'LastPFunction
    PTT' 'False 'True = 'LastPTerm
    PTT' 'True 'False = 'PFunction
    PTT' 'False 'False = 'PTerm

-- | @since 2.0.0
type PTT (p :: S -> Type) = PTT' (IsPLam p) (IsLast p)

-- | @since 2.0.0
type family IsPLam (p :: S -> Type) :: Bool where
    IsPLam ((a :--> b) :--> c) = 'True
    IsPLam _ = 'False

-- | @since 2.0.0
type family IsLast (p :: S -> Type) :: Bool where
    IsLast (a :--> PBool) = 'True
    IsLast _ = 'False

{- | Finds Haskell level TestableTerm equivlance of Plutarch
     functions. This TypeFamily expects the input Plutarch functions to
     be returning @PBool@ at the end.

     This is used to find type signatures for @quickCheck@able
     functions from Plutarch terms like @Term s (a :--> b :--> PBool)@.

 @since 2.0.0
-}
type family TestableFun (b :: PTermType) (p :: S -> Type) where
    TestableFun _ PBool = TestableTerm PBool
    TestableFun 'PTerm (a :--> b) = TestableTerm a -> TestableFun (PTT b) b
    TestableFun 'LastPTerm (a :--> b) = TestableTerm a -> TestableFun (PTT b) b
    TestableFun 'PFunction ((a :--> b) :--> c) =
        PFun a b -> TestableFun (PTT c) c
    TestableFun 'LastPFunction ((a :--> b) :--> c) =
        PFun a b -> TestableFun (PTT c) c

{- | Convert Plutarch function into testable Haskell function that takes
     `TestableTerm`. It also converts plutarch function into `PFun`.

 @since 2.0.0
-}
class FromPFunN (a :: S -> Type) (b :: S -> Type) (c :: PTermType) where
    fromPFun' ::
        Proxy c ->
        (forall s. Term s (a :--> b)) ->
        TestableFun c (a :--> b)

-- | @since 2.0.0
instance
    forall (a :: S -> Type) (b :: S -> Type).
    ( PLift a
    , PLift b
    ) =>
    FromPFunN (a :--> b) PBool 'LastPFunction
    where
    fromPFun' _ f (PFun _ _ (unTestableTerm -> x)) = TestableTerm $ f # x

-- | @since 2.0.0
instance FromPFunN a PBool 'LastPTerm where
    fromPFun' _ f (TestableTerm x) = TestableTerm $ f # x

-- | @since 2.0.0
instance
    forall
        (a :: S -> Type)
        (b :: S -> Type)
        (c :: S -> Type)
        (d :: S -> Type)
        (e :: S -> Type).
    ( c ~ (d :--> e)
    , PLift a
    , PLift b
    , FromPFunN d e (PTT c)
    ) =>
    FromPFunN (a :--> b) c 'PFunction
    where
    fromPFun' _ f (PFun _ _ (unTestableTerm -> x)) =
        fromPFun' (Proxy @(PTT c)) $ f # x

-- | @since 2.0.0
instance
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type).
    ( b ~ (c :--> d)
    , FromPFunN c d (PTT b)
    ) =>
    FromPFunN a b 'PTerm
    where
    fromPFun' _ f (TestableTerm x) = fromPFun' (Proxy @(PTT b)) $ f # x

{- | Converts Plutarch Functions into `Testable` Haskell function of
  TestableTerms.

 @since 2.0.0
-}
fromPFun ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: PTermType).
    ( c ~ PTT (a :--> b)
    , FromPFunN a b c
    ) =>
    (forall s. Term s (a :--> b)) ->
    TestableFun c (a :--> b)
fromPFun = fromPFun' (Proxy @(PTT (a :--> b)))

{- | Extracts all @TestableTerm@s from give Plutarch function.

 @since 2.0.0
-}
type family PLamArgs (p :: S -> Type) :: [Type] where
    PLamArgs (a :--> b) = TestableTerm a : PLamArgs b
    PLamArgs _ = '[]

{- | Make property by directly comparing behavior of Plutarch function
     to Haskell counterpart.  This function will expect all Plutarch
     types to be `plift`able and `pshow`able.  With given TestableTerm
     generator, it will generate value for each arguments. Then, it
     will apply generated value and lifted value to Plutarch and
     haskell function. Once all arguments are applied, It will check
     the equality of results.

 @since 2.0.0
-}
class
    (PLamArgs p ~ args) =>
    HaskEquiv
        (h :: Type)
        (p :: S -> Type)
        (args :: [Type])
    where
    haskEquiv :: h -> TestableTerm p -> NP Gen args -> Property

-- TODO: Shrinking support

-- | @since 2.0.0
instance
    forall
        (ha :: Type)
        (hb :: Type)
        (pa :: S -> Type)
        (pb :: S -> Type)
        (hbArgs :: [Type]).
    ( PLamArgs pb ~ hbArgs
    , HaskEquiv hb pb hbArgs
    , PLifted pa ~ ha
    , PLift pa
    , PShow pa
    ) =>
    HaskEquiv (ha -> hb) (pa :--> pb) (TestableTerm pa ': hbArgs)
    where
    haskEquiv h (TestableTerm p) (g :* gs) =
        forAll g $ \(TestableTerm x) -> haskEquiv (h $ plift x) (TestableTerm $ p # x) gs

-- | @since 2.0.0
instance
    forall (p :: S -> Type) (h :: Type).
    (PLamArgs p ~ '[], PLift p, PLifted p ~ h, Eq h) =>
    HaskEquiv h p '[]
    where
    haskEquiv h (TestableTerm p) _ = property $ plift p == h

{- | Simplified version of `haskEquiv`. It will use arbitrary instead of
     asking custom generators.

 @since 2.0.0
-}
haskEquiv' ::
    forall (h :: Type) (p :: S -> Type) (args :: [Type]).
    ( PLamArgs p ~ args
    , HaskEquiv h p args
    , All Arbitrary args
    ) =>
    h ->
    (forall s. Term s p) ->
    Property
haskEquiv' h p =
    haskEquiv h (TestableTerm p) $
        hcpure
            (Proxy @Arbitrary)
            arbitrary

{- | Compares evaluated UPLC

 @since 2.0.1
-}
uplcEq :: TestableTerm a -> TestableTerm b -> Property
uplcEq x y = property $ eval x == eval y
  where
    eval (TestableTerm t) =
        case compile (Config{tracingMode = NoTracing}) t of
            Left err -> error $ show err
            Right s' ->
                case evalScript s' of
                    (Right s, _, _) -> s
                    (Left err, _, _) -> error $ show err

newtype A = A Integer

{- | Placeholder for a polymorphic type. Plutarch equivalence of QuickCheck's
  `A`.

 @since 2.0.0
-}
newtype PA (s :: S)
    = PA (Term s PInteger)
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PNum)

-- | @since 2.0.0
instance DerivePlutusType PA where type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PA where type PLifted PA = A

-- | @since 2.0.0
deriving via
    (DerivePConstantViaNewtype A PA PInteger)
    instance
        PConstantDecl A

-- | @since 2.0.0
instance PArbitrary PA where
    parbitrary = do
        (TestableTerm x) <- parbitrary
        pure $ TestableTerm $ pcon $ PA x

    pshrink (TestableTerm x) =
        let f (TestableTerm y) = TestableTerm $ pcon $ PA y
         in f <$> shrink (TestableTerm $ pto x)

-- | @since 2.0.0
instance PCoArbitrary PA where
    pcoarbitrary (TestableTerm x) = pcoarbitrary $ TestableTerm $ pto x

-- | @since 2.0.0
newtype B = B Integer

{- | Same as `PA`.

 @since 2.0.0
-}
newtype PB (s :: S)
    = PB (Term s PInteger)
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PNum)

-- | @since 2.0.0
instance DerivePlutusType PB where type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PB where type PLifted PB = B

-- | @since 2.0.0
deriving via
    (DerivePConstantViaNewtype B PB PInteger)
    instance
        PConstantDecl B

-- | @since 2.0.0
instance PArbitrary PB where
    parbitrary = do
        (TestableTerm x) <- parbitrary
        pure $ TestableTerm $ pcon $ PB x

    pshrink (TestableTerm x) =
        let f (TestableTerm y) = TestableTerm $ pcon $ PB y
         in f <$> shrink (TestableTerm $ pto x)

-- | @since 2.0.0
instance PCoArbitrary PB where
    pcoarbitrary (TestableTerm x) = pcoarbitrary $ TestableTerm $ pto x

-- | @since 2.0.0
newtype C = C Integer

{- | Same as `PA`.

 @since 2.0.0
-}
newtype PC (s :: S)
    = PC (Term s PInteger)
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PNum)

-- | @since 2.0.0
instance DerivePlutusType PC where type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PC where type PLifted PC = C

-- | @since 2.0.0
deriving via
    (DerivePConstantViaNewtype C PC PInteger)
    instance
        PConstantDecl C

-- | @since 2.0.0
instance PArbitrary PC where
    parbitrary = do
        (TestableTerm x) <- parbitrary
        pure $ TestableTerm $ pcon $ PC x

    pshrink (TestableTerm x) =
        let f (TestableTerm y) = TestableTerm $ pcon $ PC y
         in f <$> shrink (TestableTerm $ pto x)

-- | @since 2.0.0
instance PCoArbitrary PC where
    pcoarbitrary (TestableTerm x) = pcoarbitrary $ TestableTerm $ pto x

{- | This shinker 'simplifies' the underlying Plutarch representation. When
     shrinking a list, this shinker is always preferable.

 @since 2.0.0
-}
shrinkPLift ::
    forall (a :: S -> Type).
    ( PLift a
    , Arbitrary (PLifted a)
    ) =>
    TestableTerm a ->
    [TestableTerm a]
shrinkPLift = fmap pconstantT . shrink . pliftT

{- | This generator uses the `Arbitrary` instance of a Haskell representation to
     make a value and lift it into Plutarch.

 @since 2.0.0
-}
arbitraryPLift ::
    forall (a :: S -> Type).
    ( PLift a
    , Arbitrary (PLifted a)
    ) =>
    Gen (TestableTerm a)
arbitraryPLift = pconstantT <$> arbitrary
