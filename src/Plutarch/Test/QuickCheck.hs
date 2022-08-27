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
    type PLamArgs,
    type PA,
    type PB,
    type PC,
    punlam',
    punlam,
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
    Equality (..),
    Partiality (..),
) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits
import Generics.SOP (All, HPure (hcpure), NP (Nil, (:*)), Proxy (Proxy))
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing), compile, tracingMode)
import Plutarch.Evaluate (evalScript, evalTerm)
import Plutarch.Lift (DerivePConstantViaNewtype (..), PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Num (PNum)
import Plutarch.Prelude (
    ClosedTerm,
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
    pconstant,
    pdata,
    plift,
    pto,
    (#),
    (#==),
    type (:-->),
 )
import Plutarch.Show (PShow)
import Plutarch.Test.QuickCheck.Function (
    PFun (..),
    pattern PFn,
 )
import Plutarch.Test.QuickCheck.Helpers (loudEval)
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
    counterexample,
    forAll,
    (.&&.),
 )

type family PUnLamHask (fin :: S -> Type) (p :: S -> Type) :: Type where
    PUnLamHask a a = TestableTerm a
    PUnLamHask fin ((a :--> b) :--> c) =
        TestableTerm (a :--> b) -> PUnLamHask fin c
    PUnLamHask fin (a :--> b) = TestableTerm a -> PUnLamHask fin b

type family CheckReturn (fin :: S -> Type) (p :: S -> Type) :: Constraint where
    CheckReturn a a = ()
    CheckReturn fin ((a :--> b) :--> c) = CheckReturn fin c
    CheckReturn fin (a :--> b) = CheckReturn fin b
    CheckReturn a b =
        ( a ~ b
        , TypeError
            ( 'Text "Return type does not match:"
                ':$$: 'Text "\tExpected \""
                ':<>: 'ShowType a
                ':<>: 'Text "\" but given function returns \""
                ':<>: 'ShowType b
                ':<>: 'Text "\""
            )
        )

type family IsFinal (fin :: S -> Type) (p :: S -> Type) :: Bool where
    IsFinal a a = 'True
    IsFinal _ _ = 'False

class PUnLam' (fin :: S -> Type) (p :: S -> Type) (end :: Bool) where
    pUnLam' :: (forall s. Term s p) -> PUnLamHask fin p

instance
    forall (fin :: S -> Type) (pa :: S -> Type) (pb :: S -> Type).
    ( PUnLam' fin pb (IsFinal fin pb)
    , PUnLamHask fin (pa :--> pb) ~ (TestableTerm pa -> PUnLamHask fin pb)
    ) =>
    PUnLam' fin (pa :--> pb) 'False
    where
    pUnLam' f = \(TestableTerm y) -> pUnLam' @fin @pb @(IsFinal fin pb) (f # y)

instance PUnLam' fin fin 'True where
    pUnLam' x = TestableTerm x

type PUnLam fin p = (PUnLam' fin p (IsFinal fin p), CheckReturn fin p)

punlam' ::
    forall (fin :: S -> Type) (p :: S -> Type).
    PUnLam fin p =>
    (forall s. Term s p) ->
    PUnLamHask fin p
punlam' = pUnLam' @fin @p @(IsFinal fin p)

punlam :: 
    forall (fin :: S -> Type) (p :: S -> Type).
    PUnLam fin p =>
    (forall s. Term s p) ->
    PUnLamHask fin p
punlam pf = punlam' @fin (loudEval pf)

{- | Converts Plutarch Functions into `Testable` Haskell function of
  TestableTerms.

 @since 2.0.0
-}
fromPFun ::
    forall (p :: S -> Type).
    (PUnLam PBool p) =>
    ClosedTerm p ->
    PUnLamHask PBool p
fromPFun pf = punlam @PBool pf

{- | Ways an Plutarch terms can be compared.
     @OnPEq@ uses Plutarch `PEq` instance to compare give terms. This
     means two terms with different UPLC representations can be
     considered equal when `PEq` instance defines so.
     @OnUPLC@ uses compiled and evaluated raw UPLC to compare two
     terms. It is useful comparing Terms that forgot their types--
     `POpqaue`.

 @since 2.1.0
-}
data Equality
    = OnPEq
    | OnPData
    | OnBoth
    deriving stock (Eq, Show)

{- | Partiality of the comparison. @ByPartial@ will have some
     performance disadventages.

 @since 2.1.0
-}
data Partiality
    = ByComplete
    | ByPartial

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
        (e :: Equality)
        (par :: Partiality)
        (h :: Type)
        (p :: S -> Type)
        (args :: [Type])
    where
    haskEquiv :: h -> TestableTerm p -> NP Gen args -> Property

-- | @since 2.1.0
instance
    forall
        (e :: Equality)
        (par :: Partiality)
        (ha :: Type)
        (hb :: Type)
        (pa :: S -> Type)
        (pb :: S -> Type)
        (hbArgs :: [Type]).
    ( PLamArgs pb ~ hbArgs
    , HaskEquiv e par hb pb hbArgs
    , PLifted pa ~ ha
    , PLift pa
    , PShow pa
    ) =>
    HaskEquiv e par (ha -> hb) (pa :--> pb) (TestableTerm pa ': hbArgs)
    where
    haskEquiv h (TestableTerm p) (g :* gs) =
        forAll g $ \(TestableTerm x) -> haskEquiv @e @par (h $ plift x) (TestableTerm $ p # x) gs

instance
    forall (e :: Equality) (p :: S -> Type) (h :: Type).
    (HaskEquiv e 'ByComplete h p '[]) =>
    HaskEquiv e 'ByPartial (Maybe h) p '[]
    where
    haskEquiv h (TestableTerm p) _ =
        case evalTerm (Config{tracingMode = DoTracing}) p of
            Left err ->
                case h of
                    Just _ -> failWith $ "Haskell expected success, but Plutarch compilation failed.\n" <> show err
                    Nothing -> property True
            Right (Left err, _, t) ->
                case h of
                    Just _ -> failWith $ "Haskell expected success, but Plutarch evaluation failed.\n" <> show err <> "\n" <> show t
                    Nothing -> property True
            Right (Right p', _, t) ->
                case h of
                    Just h' -> haskEquiv @e @( 'ByComplete) h' (TestableTerm p') Nil
                    Nothing -> failWith $ "Haskell expected failure, but Plutarch succeed.\n" <> show t

-- | @since 2.1.0
instance
    forall (p :: S -> Type) (h :: Type).
    (PLamArgs p ~ '[], PLift p, PLifted p ~ h, PEq p) =>
    HaskEquiv 'OnPEq 'ByComplete h p '[]
    where
    haskEquiv h (TestableTerm p) _ =
        counterexample "Comparison by PEq Failed" $
            property $ plift $ p #== (pconstant h)

-- | @since 2.1.0
instance
    forall (p :: S -> Type) (h :: Type).
    (PLamArgs p ~ '[], PLift p, PLifted p ~ h, PIsData p) =>
    HaskEquiv 'OnPData 'ByComplete h p '[]
    where
    haskEquiv h (TestableTerm p) _ =
        counterexample "Comparison by PData Failed" $
            property $ plift (pdata p #== pdata (pconstant h))

-- | @since 2.1.0
instance
    forall (p :: S -> Type) (h :: Type).
    (PLamArgs p ~ '[], PLift p, PLifted p ~ h, PIsData p, PEq p) =>
    HaskEquiv 'OnBoth 'ByComplete h p '[]
    where
    haskEquiv h p _ =
        (haskEquiv @( 'OnPEq) @( 'ByComplete) h p Nil) .&&. (haskEquiv @( 'OnPData) @( 'ByComplete) h p Nil)

{- | Simplified version of `haskEquiv`. It will use arbitrary instead of
     asking custom generators.

 @since 2.0.0
-}
haskEquiv' ::
    forall (e :: Equality) (par :: Partiality) (h :: Type) (p :: S -> Type) (args :: [Type]).
    ( PLamArgs p ~ args
    , HaskEquiv e par h p args
    , All Arbitrary args
    ) =>
    h ->
    (forall s. Term s p) ->
    Property
haskEquiv' h p =
    haskEquiv @e @par h (TestableTerm p) $
        hcpure (Proxy @Arbitrary) arbitrary

{- | Compares evaluated UPLC

 @since 2.0.1
-}
uplcEq :: TestableTerm a -> TestableTerm b -> Property
uplcEq x y = either failWith property go
  where
    go = do
        x' <- eval x
        y' <- eval y
        return $ x' == y'
    eval (TestableTerm t) =
        case compile (Config{tracingMode = NoTracing}) t of
            Left err -> Left $ "Term compilation failed:\n" <> show err
            Right s' ->
                case evalScript s' of
                    (Right s, _, _) -> Right s
                    (Left err, _, _) -> Left $ "Term evaluation failed:\n" <> show err

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

-- Utilities
failWith :: String -> Property
failWith err = counterexample err $ property False
