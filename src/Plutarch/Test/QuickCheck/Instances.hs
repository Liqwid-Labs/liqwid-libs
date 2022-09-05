{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck.Instances (
  TestableTerm (..),
  PArbitrary (..),
  PCoArbitrary (..),
  pconstantT,
  pliftT,
) where

import Data.ByteString (ByteString)
import qualified Data.Text as T (intercalate, pack, unpack)
import qualified GHC.Exts as Exts (IsList (fromList, toList))
import Plutarch (
  Config (..),
  PlutusType,
  S,
  Term,
  TracingMode (DoTracing),
  compile,
  pcon,
  pmatch,
  pto,
  (#),
  (#$),
 )
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PCurrencySymbol (PCurrencySymbol),
  PExtended (PFinite, PNegInf, PPosInf),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PMap (PMap),
  PStakeValidatorHash (PStakeValidatorHash),
  PTokenName (PTokenName),
  PUpperBound (PUpperBound),
  PValidatorHash (PValidatorHash),
  PValue (PValue),
 )
import Plutarch.Api.V1.Time (PPOSIXTime (PPOSIXTime))
import Plutarch.Api.V1.Tuple (
  PTuple,
  pbuiltinPairFromTuple,
  ptuple,
  ptupleFromBuiltin,
 )
import Plutarch.Api.V2 (
  AmountGuarantees (NoGuarantees),
  KeyGuarantees (Unsorted),
  PAddress (PAddress),
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash (PPubKeyHash),
  PStakingCredential (PStakingHash, PStakingPtr),
 )
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.Maybe (
  pfromDJust,
  pisDJust,
  pisJust,
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted), plift)
import Plutarch.Maybe (pfromJust)
import Plutarch.Positive (PPositive, ptryPositive)
import Plutarch.Prelude (
  PAsData,
  PBool,
  PBuiltinList,
  PBuiltinPair,
  PByteString,
  PEither (PLeft, PRight),
  PInteger,
  PIsData,
  PIsListLike,
  PLift,
  PList,
  PListLike (pcons, phead, pnil, pnull, ptail),
  PMaybe (PJust, PNothing),
  PPair (PPair),
  PPartialOrd ((#<)),
  PRational (PRational),
  PString,
  PUnit,
  Type,
  pconstant,
  pdata,
  pdcons,
  pdenominator,
  pdnil,
  pfield,
  pfromData,
  pif,
  pnumerator,
  pshow,
  ptraceError,
 )
import Plutarch.Show (PShow)
import Plutarch.Test.QuickCheck.Helpers (loudEval)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  Testable (property),
  chooseInt,
  elements,
  frequency,
  functionMap,
  sized,
  variant,
  vectorOf,
 )

-- | @since 2.0.0
instance Testable (TestableTerm PBool) where
  property (TestableTerm t) = property (plift t)

{- | TestableTerm is a wrapper for closed Plutarch terms. This
     abstraction allows Plutarch values to be generated via QuickCheck
     generators.

     = Note
     The typechecker is picky about how @TestableTerm@s are constructed.
     Meaning, TestableTerm can throw an error when it's composed.

 @since 2.0.0
-}
data TestableTerm a = TestableTerm
  { unTestableTerm :: forall s. Term s a
  -- ^ Converts @TestableTerm@ back into Plutarch @ClosedTerm@
  -- @since 2.0.0
  }

-- | @since 2.0.0
instance (forall (s :: S). Num (Term s a)) => Num (TestableTerm a) where
  (+) = liftT2 (+)
  (*) = liftT2 (*)
  abs = liftT abs
  negate = liftT negate
  signum = liftT signum
  fromInteger i = TestableTerm (fromInteger i :: Term s a)

{- | For any Plutarch Type that have a `PShow` instance, `Show` is
     available as well. For those that don't have @PShow@ instances,
     we have to use `forAllShow` with custom display function to
     execute a property check.

 @since 2.0.0
-}
instance PShow a => Show (TestableTerm a) where
  show (TestableTerm term) =
    case compile (Config {tracingMode = DoTracing}) $
      ptraceError
        (pshow term) of
      Left err -> show err
      Right (evalScript -> (_, _, trace)) ->
        T.unpack . T.intercalate " " $ trace

{- | PArbitrary is the Plutarch equivalent of the `Arbitrary` typeclass from
     QuickCheck. It generates pseudo-random closed term, which can be used
     to test properties over Plutarch code without having to compile and
     evaluate.

     Default implmentations are given for any Plutarch type that
     implements @PLift a@ and @Arbitrary (PLifted a)@. This generates
     a Haskell value and converts it into a Plutarch term using `pconstant`.

     = Note

     The default implementation for 'pshrink' does no shrinking. If at all
     possible, please define a shrinker, as this will make your test results
     much more useful.

 @since 2.0.0
-}
class PArbitrary (a :: S -> Type) where
  parbitrary :: Gen (TestableTerm a)
  default parbitrary ::
    (PLift a, Arbitrary (PLifted a)) =>
    Gen (TestableTerm a)
  parbitrary = pconstantT <$> arbitrary

  pshrink :: TestableTerm a -> [TestableTerm a]
  pshrink = const []

class PCoArbitrary (a :: S -> Type) where
  pcoarbitrary :: TestableTerm a -> Gen b -> Gen b

{- | Any Plutarch type that implements `PArbitrary` automatically has an
     instance of `Arbitrary` for its @TestableTerm@. This allows interfacing
     between QuickCheck and Plutarch.

     = Note

     The default implementation for 'pshrink' does no shrinking. If at all
     possible, please define a shrinker, as this will make your test results
     much more useful.

 @since 2.0.0
-}
instance PArbitrary p => Arbitrary (TestableTerm p) where
  arbitrary = parbitrary
  shrink = pshrink . (\(TestableTerm x) -> TestableTerm $ loudEval x)

instance PCoArbitrary p => CoArbitrary (TestableTerm p) where
  coarbitrary = pcoarbitrary

-- | @since 2.0.0
instance (PArbitrary p, PIsData p) => PArbitrary (PAsData p) where
  parbitrary = pdataT <$> parbitrary
  pshrink = fmap pdataT . shrink . pfromDataT

instance (PCoArbitrary p, PIsData p) => PCoArbitrary (PAsData p) where
  pcoarbitrary (pfromDataT -> x) = pcoarbitrary x

instance Function (TestableTerm PInteger) where
  function = functionMap pliftT pconstantT

-- | @since 2.0.0
instance PArbitrary PInteger where
  pshrink = fmap pconstantT . shrink . pliftT

instance PCoArbitrary PInteger where
  pcoarbitrary (pliftT -> x) = coarbitrary x

-- | @since 2.0.0
instance PArbitrary PBool where
  pshrink = fmap pconstantT . shrink . pliftT

instance PCoArbitrary PBool where
  pcoarbitrary (pliftT -> x) = coarbitrary x

-- | @since 2.0.0
instance PArbitrary PUnit where
  pshrink = fmap pconstantT . shrink . pliftT

instance PCoArbitrary PUnit where
  pcoarbitrary (pliftT -> x) = coarbitrary x

-- | @since 2.0.0
instance PArbitrary PByteString where
  parbitrary = sized $ \r -> do
    len <- chooseInt (0, r)
    bs <- genByteString len
    return $ pconstantT bs

  pshrink = fmap pconstantT . shrinkByteString . pliftT

-- | @since 2.0.0
instance PCoArbitrary PByteString where
  pcoarbitrary = coarbitrary . sum . Exts.toList . pliftT

-- | @since 2.0.0
instance PArbitrary PPositive where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    return $ TestableTerm $ ptryPositive #$ pif (0 #< x) x (negate x + 1)

-- | @since 2.0.0
instance PCoArbitrary PPositive where
  pcoarbitrary = pcoarbitrary . liftT pto

-- | @since 2.0.0
instance PArbitrary PRational where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    (TestableTerm y) <- parbitrary
    return $ pconT $ PRational x y

  pshrink (TestableTerm x) =
    [ TestableTerm $ pcon $ PRational a (pdenominator # x)
    | (TestableTerm a) <- shrink (TestableTerm $ pnumerator # x)
    ]

instance PCoArbitrary PRational where
  pcoarbitrary (TestableTerm x) = pcoarbitrary n . pcoarbitrary d
    where
      n = TestableTerm $ pnumerator # x
      d = TestableTerm $ pdenominator # x

-- | @since 2.0.0
instance PArbitrary PString where
  parbitrary = pconstantT . T.pack <$> arbitrary
  pshrink = fmap (pconstantT . T.pack) . shrink . T.unpack . pliftT

instance PCoArbitrary PString where
  pcoarbitrary = coarbitrary . T.unpack . pliftT

-- | @since 2.0.0
instance PArbitrary a => PArbitrary (PMaybe a) where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    frequency
      [ (3, return $ TestableTerm $ pcon $ PJust x)
      ,
        ( 1
        , return $
            pconT PNothing
        )
      ]
  pshrink (TestableTerm x)
    | plift $ pisJust # x =
        TestableTerm (pcon PNothing) :
          [ TestableTerm $ pcon $ PJust a
          | (TestableTerm a) <- shrink (TestableTerm $ pfromJust # x)
          ]
    | otherwise = []

instance PCoArbitrary a => PCoArbitrary (PMaybe a) where
  pcoarbitrary (TestableTerm x)
    | plift $ pisJust # x =
        variant (1 :: Integer)
          . pcoarbitrary
            (TestableTerm $ pfromJust # x)
    | otherwise = variant (0 :: Integer)

-- | @since 2.0.0
instance (PIsData a, PArbitrary a) => PArbitrary (PMaybeData a) where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    elements
      [ TestableTerm $ pcon $ PDJust $ pdcons @"_0" # pdata x # pdnil
      , TestableTerm $ pcon $ PDNothing pdnil
      ]
  pshrink (TestableTerm x)
    | plift $ pisDJust # x =
        pconT (PDNothing pdnil) :
          [ TestableTerm $ pcon $ PDJust $ pdcons @"_0" # pdata a # pdnil
          | (TestableTerm a) <- shrink (TestableTerm $ pfromDJust # x)
          ]
    | otherwise = []

instance (PIsData a, PCoArbitrary a) => PCoArbitrary (PMaybeData a) where
  pcoarbitrary (TestableTerm x)
    | plift $ pisDJust # x =
        variant (1 :: Integer)
          . pcoarbitrary
            (TestableTerm $ pfromDJust # x)
    | otherwise = variant (0 :: Integer)

-- | @since 2.0.0
instance (PArbitrary a, PArbitrary b) => PArbitrary (PEither a b) where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    (TestableTerm y) <- parbitrary
    elements [TestableTerm $ pcon $ PRight x, TestableTerm $ pcon $ PLeft y]

  pshrink x
    | pliftT $ isRight x =
        [ pconT $ PRight a
        | (TestableTerm a) <- shrink (pright x)
        ]
    | otherwise =
        [ pconT $ PLeft a
        | (TestableTerm a) <- shrink (pleft x)
        ]

instance (PCoArbitrary a, PCoArbitrary b) => PCoArbitrary (PEither a b) where
  pcoarbitrary x
    | pliftT $ isRight x = variant (0 :: Integer) . pcoarbitrary (pright x)
    | otherwise = variant (1 :: Integer) . pcoarbitrary (pleft x)

{- | Unfortunately, it is impossible to create @PBuiltinPair@ at the
     Plutarch level without getting into manipulating raw Plutus
     data. Instead, it can only be created from haskell level value
     and constanted.

     This limitation limites this generator to only accepting Plutarch
     types that have @PLift@ and @Arbitrary (PLifted a)@.

 @since 2.0.0
-}
instance
  ( PLift a
  , PLift b
  , Arbitrary (PLifted a, PLifted b)
  ) =>
  PArbitrary (PBuiltinPair a b)
  where
  parbitrary = pconstantT <$> arbitrary

  pshrink = fmap pconstantT . shrink . pliftT

instance
  ( PLift a
  , PLift b
  , CoArbitrary (PLifted a, PLifted b)
  ) =>
  PCoArbitrary (PBuiltinPair a b)
  where
  pcoarbitrary = coarbitrary . pliftT

-- | @since 2.0.0
instance
  {-# OVERLAPPING #-}
  ( PArbitrary a
  , PArbitrary b
  , PIsData a
  , PIsData b
  ) =>
  PArbitrary (PBuiltinPair (PAsData a) (PAsData b))
  where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    return $ TestableTerm $ pfromData $ pbuiltinPairFromTuple (pdata x)

  pshrink = fmap fromTuple . shrink . toTuple
    where
      toTuple = liftT (pfromData . ptupleFromBuiltin . pdata)
      fromTuple = liftT (pfromData . pbuiltinPairFromTuple . pdata)

instance
  {-# OVERLAPPING #-}
  ( PCoArbitrary a
  , PCoArbitrary b
  , PIsData a
  , PIsData b
  ) =>
  PCoArbitrary (PBuiltinPair (PAsData a) (PAsData b))
  where
  pcoarbitrary (liftT ptupleFromBuiltin . pdataT -> t) = pcoarbitrary t

-- | @since 2.0.0
instance
  ( PArbitrary a
  , PArbitrary b
  ) =>
  PArbitrary (PPair a b)
  where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    (TestableTerm y) <- parbitrary
    return . pconT $ PPair x y

  pshrink x =
    [ pconT $ PPair a b
    | (TestableTerm a) <- shrink $ ppFstT x
    , (TestableTerm b) <- shrink $ ppSndT x
    ]

instance (PCoArbitrary a, PCoArbitrary b) => PCoArbitrary (PPair a b) where
  pcoarbitrary x = pcoarbitrary (ppFstT x) . pcoarbitrary (ppSndT x)

-- | @since 2.0.0
instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PArbitrary a
  , PArbitrary b
  , PIsData a
  , PIsData b
  ) =>
  PArbitrary (PTuple a b)
  where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    (TestableTerm y) <- parbitrary
    return $ TestableTerm $ ptuple # pdata x # pdata y

  pshrink x =
    [ TestableTerm $ ptuple # a # b
    | (TestableTerm a) <- shrink $ ptFstT x
    , (TestableTerm b) <- shrink $ ptSndT x
    ]

instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PCoArbitrary a
  , PCoArbitrary b
  , PIsData a
  , PIsData b
  ) =>
  PCoArbitrary (PTuple a b)
  where
  pcoarbitrary x = pcoarbitrary (ptFstT x) . pcoarbitrary (ptSndT x)

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PArbitrary a, PIsListLike PBuiltinList a) =>
  PArbitrary (PBuiltinList a)
  where
  parbitrary = constrPList <$> arbitrary
  pshrink = shrinkPListLike

instance
  forall (a :: S -> Type).
  (PCoArbitrary a, PIsListLike PBuiltinList a) =>
  PCoArbitrary (PBuiltinList a)
  where
  pcoarbitrary = coArbitraryPListLike

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PArbitrary a, PIsListLike PList a) =>
  PArbitrary (PList a)
  where
  parbitrary = genPListLike
  pshrink = shrinkPListLike

instance (PCoArbitrary a, PIsListLike PList a) => PCoArbitrary (PList a) where
  pcoarbitrary = coArbitraryPListLike

-- | @since 2.0.0
instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PArbitrary a
  , PArbitrary b
  , PIsData a
  , PIsData b
  ) =>
  PArbitrary (PMap 'Unsorted a b)
  where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    return $ pconT $ PMap x

  pshrink = fmap reMap . shrink . unMap
    where
      reMap (TestableTerm x) = pconT $ PMap x
      unMap = flip pmatchT $ \(PMap a) -> a

-- | @since 2.0.0
instance
  forall (a :: S -> Type) (b :: S -> Type) (c :: KeyGuarantees).
  (PCoArbitrary a, PCoArbitrary b, PIsData a, PIsData b) =>
  PCoArbitrary (PMap c a b)
  where
  pcoarbitrary = pcoarbitrary . unMap
    where
      unMap = flip pmatchT $ \(PMap a) -> a

-- | @since 2.0.0
instance PArbitrary PPOSIXTime where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    return $ pconT $ PPOSIXTime x

  pshrink = fmap (\(TestableTerm x) -> pconT $ PPOSIXTime x) . shrink . unTime
    where
      unTime = flip pmatchT $ \(PPOSIXTime a) -> a

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PIsData a, PArbitrary a) =>
  PArbitrary (PExtended a)
  where
  parbitrary = do
    (TestableTerm x) <- parbitrary
    elements
      [ pconT $ PNegInf pdnil
      , pconT $ PFinite $ pdcons @"_0" # pdata x # pdnil
      , pconT $ PPosInf pdnil
      ]

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PIsData a, PArbitrary a) =>
  PArbitrary (PLowerBound a)
  where
  parbitrary = do
    (TestableTerm ex) <- parbitrary
    (TestableTerm cl) <- parbitrary
    return $
      pconT $
        PLowerBound $
          pdcons @"_0" # pdata ex #$ pdcons @"_1" # pdata cl # pdnil

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PIsData a, PArbitrary a) =>
  PArbitrary (PUpperBound a)
  where
  parbitrary = do
    (TestableTerm ex) <- parbitrary
    (TestableTerm cl) <- parbitrary
    return $
      pconT $
        PUpperBound $
          pdcons @"_0" # pdata ex #$ pdcons @"_1" # pdata cl # pdnil

-- | @since 2.0.0
instance
  forall (a :: S -> Type).
  (PIsData a, PArbitrary a) =>
  PArbitrary (PInterval a)
  where
  parbitrary = do
    (TestableTerm lo) <- parbitrary
    (TestableTerm up) <- parbitrary
    return $
      pconT $
        PInterval $
          pdcons @"from" # pdata lo #$ pdcons @"to" # pdata up # pdnil

-- | @since 2.0.0
instance PArbitrary PPubKeyHash where
  parbitrary = do
    -- PubKeyHash should be 28 bytes long
    bs <- genByteString 28
    return $ pconT $ PPubKeyHash $ pconstant bs

-- | @since 2.0.0
instance PArbitrary PValidatorHash where
  parbitrary = do
    -- ValidatorHash should be 28 bytes long
    bs <- genByteString 28
    return $ pconT $ PValidatorHash $ pconstant bs

-- | @since 2.0.0
instance PArbitrary PStakeValidatorHash where
  parbitrary = do
    -- StakeValidatorHash should be 28 bytes long
    bs <- genByteString 28
    return $ pconT $ PStakeValidatorHash $ pconstant bs

-- | @since 2.0.0
instance PArbitrary PCredential where
  parbitrary = do
    (TestableTerm pk) <- parbitrary
    (TestableTerm vh) <- parbitrary
    elements
      [ pconT $ PScriptCredential $ pdcons @"_0" # pdata vh # pdnil
      , pconT $ PPubKeyCredential $ pdcons @"_0" # pdata pk # pdnil
      ]

-- | @since 2.0.0
instance PArbitrary PStakingCredential where
  parbitrary = do
    (TestableTerm cred) <- parbitrary
    (TestableTerm x) <- parbitrary
    (TestableTerm y) <- parbitrary
    (TestableTerm z) <- parbitrary
    elements
      [ pconT $ PStakingHash $ pdcons @"_0" # pdata cred # pdnil
      , pconT $
          PStakingPtr $
            pdcons @"_0" # pdata x
              #$ pdcons @"_1" # pdata y
              #$ pdcons @"_2" # pdata z # pdnil
      ]

-- | @since 2.0.0
instance PArbitrary PAddress where
  parbitrary = do
    (TestableTerm cred) <- parbitrary
    (TestableTerm scred) <- parbitrary
    return $
      pconT $
        PAddress $
          pdcons @"credential" # pdata cred
            #$ pdcons @"stakingCredential" # pdata scred # pdnil

-- | @since 2.0.0
instance PArbitrary PCurrencySymbol where
  parbitrary = do
    (TestableTerm cs) <- parbitrary
    return $ pconT $ PCurrencySymbol cs

-- | @since 2.0.0
instance PArbitrary PTokenName where
  parbitrary = do
    len <- chooseInt (1, 32)
    tn <- genByteString len
    return $ pconT $ PTokenName $ pconstant tn

-- | @since 2.0.0
instance PArbitrary (PValue 'Unsorted 'NoGuarantees) where
  parbitrary = do
    (TestableTerm val) <- parbitrary
    return $ pconT $ PValue val

  pshrink = fmap (\(TestableTerm x) -> pconT $ PValue x) . shrink . unValue
    where
      unValue = flip pmatchT $ \(PValue a) -> a

------------------------------------------------------------
-- Helpers

genByteString :: Int -> Gen ByteString
genByteString l = Exts.fromList <$> vectorOf l arbitrary

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString bs = do
  xs' <- shrink . Exts.toList $ bs
  pure . Exts.fromList $ xs'

isRight ::
  forall
    {a :: S -> Type}
    {b :: S -> Type}.
  TestableTerm (PEither a b) ->
  TestableTerm PBool
isRight = flip pmatchT $ \case
  PRight _ -> pconstant True
  _ -> pconstant False

pright ::
  forall
    {a :: S -> Type}
    {b :: S -> Type}.
  TestableTerm (PEither a b) ->
  TestableTerm b
pright = flip pmatchT $ \case
  PRight a -> a
  _ -> ptraceError "asked for PRight when it is PLeft"

pleft ::
  forall
    {b1 :: S -> Type}
    {b2 :: S -> Type}.
  TestableTerm (PEither b1 b2) ->
  TestableTerm b1
pleft = flip pmatchT $ \case
  PLeft a -> a
  _ -> ptraceError "asked for PLeft when it is PRight"

-- | @since 2.0.0
liftT ::
  forall (a :: S -> Type) (b :: S -> Type).
  (forall (s :: S). Term s a -> Term s b) ->
  TestableTerm a ->
  TestableTerm b
liftT f (TestableTerm x) = TestableTerm $ f x

-- | @since 2.0.0
liftT2 ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  (forall (s :: S). Term s a -> Term s b -> Term s c) ->
  TestableTerm a ->
  TestableTerm b ->
  TestableTerm c
liftT2 f (TestableTerm x) (TestableTerm y) = TestableTerm $ f x y

ppFstT ::
  forall {a :: S -> Type} {b :: S -> Type}.
  TestableTerm (PPair a b) ->
  TestableTerm a
ppFstT = flip pmatchT $ \(PPair a _) -> a

ppSndT ::
  forall {a :: S -> Type} {b :: S -> Type}.
  TestableTerm (PPair a b) ->
  TestableTerm b
ppSndT = flip pmatchT $ \(PPair _ a) -> a

pdataT ::
  forall {p :: S -> Type}.
  PIsData p =>
  TestableTerm p ->
  TestableTerm (PAsData p)
pdataT (TestableTerm x) = TestableTerm $ pdata x

pfromDataT ::
  forall {p :: S -> Type}.
  PIsData p =>
  TestableTerm (PAsData p) ->
  TestableTerm p
pfromDataT (TestableTerm x) = TestableTerm $ pfromData x

pliftT ::
  forall {p :: S -> Type} {h :: Type}.
  (PLift p, PLifted p ~ h) =>
  TestableTerm p ->
  h
pliftT (TestableTerm x) = plift x

pconstantT ::
  forall {p :: S -> Type} {h :: Type}.
  (PLift p, PLifted p ~ h) =>
  h ->
  TestableTerm p
pconstantT h = TestableTerm $ pconstant h

pconT ::
  forall {p :: S -> Type}.
  PlutusType p =>
  (forall {s :: S}. p s) ->
  TestableTerm p
pconT p = TestableTerm $ pcon p

pmatchT ::
  forall {p :: S -> Type} {b :: S -> Type}.
  PlutusType p =>
  TestableTerm p ->
  (forall {s :: S}. p s -> Term s b) ->
  TestableTerm b
pmatchT (TestableTerm p) f = TestableTerm $ pmatch p f

ptFstT ::
  forall {a :: S -> Type} {b :: S -> Type}.
  (PIsData a) =>
  TestableTerm (PTuple a b) ->
  TestableTerm (PAsData a)
ptFstT = liftT (pfield @"_0" #)

ptSndT ::
  forall {a :: S -> Type} {b :: S -> Type}.
  (PIsData b) =>
  TestableTerm (PTuple a b) ->
  TestableTerm (PAsData b)
ptSndT = liftT (pfield @"_1" #)

constrPList ::
  forall {a :: S -> Type} {b :: (S -> Type) -> S -> Type}.
  (PIsListLike b a) =>
  [TestableTerm a] ->
  TestableTerm (b a)
constrPList [] = TestableTerm pnil
constrPList ((TestableTerm x) : xs) =
  let (TestableTerm rest) = constrPList xs
   in TestableTerm $ pcons # x # rest

convToList ::
  forall {a :: S -> Type} {b :: (S -> Type) -> S -> Type}.
  (PIsListLike b a) =>
  TestableTerm (b a) ->
  [TestableTerm a]
convToList (TestableTerm x)
  | not $ plift $ pnull # x =
      TestableTerm (phead # x) : convToList (TestableTerm $ ptail # x)
  | otherwise = []

genPListLike ::
  forall {a :: S -> Type} {b :: (S -> Type) -> S -> Type}.
  (PArbitrary a, PIsListLike b a) =>
  Gen (TestableTerm (b a))
genPListLike = constrPList <$> arbitrary

shrinkPListLike ::
  forall {a :: S -> Type} {b :: (S -> Type) -> S -> Type}.
  ( PArbitrary a
  , PIsListLike b a
  ) =>
  TestableTerm (b a) ->
  [TestableTerm (b a)]
shrinkPListLike xs' = constrPList <$> shrink (convToList xs')

coArbitraryPListLike ::
  forall {a :: S -> Type} {b :: (S -> Type) -> S -> Type} {c :: Type}.
  (PCoArbitrary a, PCoArbitrary (b a), PIsListLike b a) =>
  TestableTerm (b a) ->
  Gen c ->
  Gen c
coArbitraryPListLike (TestableTerm x)
  | plift (pnull # x) = variant (0 :: Integer)
  | otherwise =
      variant (1 :: Integer) . pcoarbitrary (TestableTerm $ phead # x)
        . pcoarbitrary (TestableTerm $ ptail # x)
