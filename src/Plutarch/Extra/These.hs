{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.These (
  -- * Types
  PThese (..),
  PDThese (..),

  -- * Functions
  pthese,
  pdthese,
  toPDThese,
  fromPDThese,
) where

import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Bool (pcompare)
import Plutarch.Extra.Boring (pboring)
import Plutarch.Extra.Functor (
  PBifunctor (PSubcategoryLeft, PSubcategoryRight, pbimap, psecond),
  PFunctor (PSubcategory, pfmap),
  Plut,
 )
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Extra.Traversable (PTraversable (ptraverse, ptraverse_))

{- | A data type which contains an @a@, a @b@, or both. This uses a
 Scott-encoded representation.

 @since 1.0.0
-}
data PThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThis (Term s a)
  | PThat (Term s b)
  | PThese (Term s a) (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType (PThese a b) where type DPTStrat _ = PlutusTypeScott

-- | @since 1.0.0
instance (POrd a, POrd b) => PPartialOrd (PThese a b) where
  t1 #<= t2 = unTermCont $ do
    t1' <- pmatchC t1
    t2' <- pmatchC t2
    pure $ case (t1', t2') of
      (PThis ta, PThis ta') -> ta #<= ta'
      (PThis _, _) -> pcon PTrue
      (PThat _, PThis _) -> pcon PFalse
      (PThat tb, PThat tb') -> tb #<= tb'
      (PThat _, _) -> pcon PTrue
      (PThese _ _, PThis _) -> pcon PFalse
      (PThese _ _, PThat _) -> pcon PFalse
      (PThese ta tb, PThese ta' tb') ->
        pcompare ta ta' (pcon PTrue) (tb #<= tb') (pcon PFalse)
  t1 #< t2 = unTermCont $ do
    t1' <- pmatchC t1
    t2' <- pmatchC t2
    pure $ case (t1', t2') of
      (PThis ta, PThis ta') -> ta #< ta'
      (PThis _, _) -> pcon PTrue
      (PThat _, PThis _) -> pcon PFalse
      (PThat tb, PThat tb') -> tb #< tb'
      (PThat _, _) -> pcon PTrue
      (PThese _ _, PThis _) -> pcon PFalse
      (PThese _ _, PThat _) -> pcon PFalse
      (PThese ta tb, PThese ta' tb') ->
        pcompare ta ta' (pcon PTrue) (tb #< tb') (pcon PFalse)

-- | @since 1.0.0
deriving anyclass instance (PShow a, PShow b) => PShow (PThese a b)

-- | @since 3.1.0
instance PFunctor (PThese a) where
  type PSubcategory (PThese a) = Plut
  pfmap = psecond

-- | @since 3.1.0
instance PBifunctor PThese where
  type PSubcategoryLeft PThese = Plut
  type PSubcategoryRight PThese = Plut
  pbimap = phoistAcyclic $
    plam $ \f g t -> unTermCont $ do
      t' <- pmatchC t
      pure $ case t' of
        PThis ta -> pcon . PThis $ f # ta
        PThat tb -> pcon . PThat $ g # tb
        PThese ta tb -> pcon . PThese (f # ta) $ g # tb

-- | @since 1.0.0
instance (forall (s :: S). Semigroup (Term s a)) => PApply (PThese a) where
  pliftA2 = phoistAcyclic $
    plam $ \f xs ys -> unTermCont $ do
      xs' <- pmatchC xs
      ys' <- pmatchC ys
      pure . pcon $ case (xs', ys') of
        (PThis ta, PThis ta') -> PThis (ta <> ta')
        (PThis ta, PThat _) -> PThis ta
        (PThis ta, PThese ta' _) -> PThis (ta <> ta')
        (PThat _, PThis ta') -> PThis ta'
        (PThat tb, PThat tb') -> PThat $ f # tb # tb'
        (PThat tb, PThese ta' tb') -> PThese ta' $ f # tb # tb'
        (PThese ta _, PThis ta') -> PThis (ta <> ta')
        (PThese ta tb, PThat tb') -> PThese ta $ f # tb # tb'
        (PThese ta tb, PThese ta' tb') -> PThese (ta <> ta') $ f # tb # tb'

-- | @since 1.0.0
instance (forall (s :: S). Semigroup (Term s a)) => PApplicative (PThese a) where
  ppure = phoistAcyclic $ plam $ pcon . PThat

-- | @since 1.0.0
instance
  (forall (s' :: S). Semigroup (Term s' a), forall (s' :: S). Semigroup (Term s' b)) =>
  Semigroup (Term s (PThese a b))
  where
  t <> t' = unTermCont $ do
    tleft <- pmatchC t
    tright <- pmatchC t'
    pure . pcon $ case (tleft, tright) of
      (PThis ta, PThis ta') -> PThis (ta <> ta')
      (PThis ta, PThat tb') -> PThese ta tb'
      (PThis ta, PThese ta' tb') -> PThese (ta <> ta') tb'
      (PThat tb, PThis ta') -> PThese ta' tb
      (PThat tb, PThat tb') -> PThat (tb <> tb')
      (PThat tb, PThese ta' tb') -> PThese ta' (tb <> tb')
      (PThese ta tb, PThis ta') -> PThese (ta <> ta') tb
      (PThese ta tb, PThat tb') -> PThese ta (tb <> tb')
      (PThese ta tb, PThese ta' tb') -> PThese (ta <> ta') (tb <> tb')

-- | @since 1.0.0
instance
  (forall (s' :: S). Monoid (Term s' a), forall (s' :: S). Monoid (Term s' b)) =>
  Monoid (Term s (PThese a b))
  where
  mempty = pcon . PThese mempty $ mempty

-- | @since 1.0.0
instance PTraversable (PThese a) where
  ptraverse = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
      t' <- pmatchC t
      pure $ case t' of
        PThis ta -> ppure # (pcon . PThis $ ta)
        PThat tb -> pfmap # plam (pcon . PThat) # (f # tb)
        PThese ta tb -> pfmap # plam (pcon . PThese ta) # (f # tb)
  ptraverse_ = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
      t' <- pmatchC t
      pure $ case t' of
        PThis _ -> ppure # pboring
        PThat tb -> f # tb
        PThese _ tb -> f # tb

{- | As 'PThese', but using a 'PlutusCore.Data.Data' encoding instead.

 @since 1.0.0
-}
data PDThese (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PDThis (Term s (PDataRecord '["_0" ':= a]))
  | PDThat (Term s (PDataRecord '["_0" ':= b]))
  | PDThese (Term s (PDataRecord '["_0" ':= a, "_1" ':= b]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

-- | @since 1.0.0
instance DerivePlutusType (PDThese a b) where type DPTStrat _ = PlutusTypeData

-- | @since 1.0.0
deriving anyclass instance (POrd a, POrd b, PIsData a, PIsData b) => PPartialOrd (PDThese a b)

-- | @since 1.0.0
deriving anyclass instance (POrd a, POrd b, PIsData a, PIsData b) => POrd (PDThese a b)

-- | @since 1.0.0
instance (PIsData a) => PFunctor (PDThese a) where
  type PSubcategory (PDThese a) = PIsData
  pfmap = psecond

-- | @since 1.0.0
instance PBifunctor PDThese where
  type PSubcategoryLeft PDThese = PIsData
  type PSubcategoryRight PDThese = PIsData
  pbimap = phoistAcyclic $
    plam $ \f g t -> unTermCont $ do
      t' <- pmatchC t
      case t' of
        PDThis ta -> do
          ta' <- pletC (pfield @"_0" # ta)
          ta'' <- pletC (pdata (f # pfromData ta'))
          pure . pcon . PDThis $ pdcons # ta'' # pdnil
        PDThat tb -> do
          tb' <- pletC (pfield @"_0" # tb)
          tb'' <- pletC (pdata (g # pfromData tb'))
          pure . pcon . PDThat $ pdcons # tb'' # pdnil
        PDThese tab -> do
          ta' <- pletC (pfield @"_0" # tab)
          ta'' <- pletC (pdata (f # pfromData ta'))
          tb' <- pletC (pfield @"_1" # tab)
          tb'' <- pletC (pdata (g # pfromData tb'))
          pure . pcon . PDThese $ pdcons # ta'' # (pdcons # tb'' # pdnil)

-- | @since 1.0.0
instance
  (PIsData a, forall (s :: S). Semigroup (Term s (PAsData a))) =>
  PApply (PDThese a)
  where
  pliftA2 = phoistAcyclic $
    plam $ \f xs ys -> unTermCont $ do
      xs' <- pmatchC xs
      ys' <- pmatchC ys
      case (xs', ys') of
        (PDThis ta, PDThis ta') -> do
          taData <- pletC (pfield @"_0" # ta)
          taData' <- pletC (pfield @"_0" # ta')
          pure . pcon . PDThis $ pdcons # (taData <> taData') # pdnil
        (PDThis ta, PDThat _) -> do
          taData <- pletC (pfield @"_0" # ta)
          pure . pcon . PDThis $ pdcons # taData # pdnil
        (PDThis ta, PDThese tab') -> do
          taData <- pletC (pfield @"_0" # ta)
          taData' <- pletC (pfield @"_0" # tab')
          pure . pcon . PDThis $ pdcons # (taData <> taData') # pdnil
        (PDThat _, PDThis ta') -> do
          taData' <- pletC (pfield @"_0" # ta')
          pure . pcon . PDThis $ pdcons # taData' # pdnil
        (PDThat tb, PDThat tb') -> do
          tbData <- pletC (pfield @"_0" # tb)
          tbData' <- pletC (pfield @"_0" # tb')
          res <- pletC (f # pfromData tbData # pfromData tbData')
          pure . pcon . PDThat $ pdcons # pdata res # pdnil
        (PDThat tb, PDThese tab') -> do
          tbData <- pletC (pfield @"_0" # tb)
          taData' <- pletC (pfield @"_0" # tab')
          tbData' <- pletC (pfield @"_1" # tab')
          res <- pletC (f # pfromData tbData # pfromData tbData')
          pure . pcon . PDThese $ pdcons # taData' # (pdcons # pdata res # pdnil)
        (PDThese tab, PDThis ta') -> do
          taData <- pletC (pfield @"_0" # tab)
          taData' <- pletC (pfield @"_0" # ta')
          pure . pcon . PDThis $ pdcons # (taData <> taData') # pdnil
        (PDThese tab, PDThat tb') -> do
          taData <- pletC (pfield @"_0" # tab)
          tbData <- pletC (pfield @"_1" # tab)
          tbData' <- pletC (pfield @"_0" # tb')
          res <- pletC (f # pfromData tbData # pfromData tbData')
          pure . pcon . PDThese $ pdcons # taData # (pdcons # pdata res # pdnil)
        (PDThese tab, PDThese tab') -> do
          taData <- pletC (pfield @"_0" # tab)
          tbData <- pletC (pfield @"_1" # tab)
          taData' <- pletC (pfield @"_0" # tab')
          tbData' <- pletC (pfield @"_1" # tab')
          res <- pletC (f # pfromData tbData # pfromData tbData')
          pure . pcon . PDThese $ pdcons # (taData <> taData') # (pdcons # pdata res # pdnil)

-- | @since 1.0.0
instance
  (forall (s :: S). Semigroup (Term s (PAsData a)), PIsData a) =>
  PApplicative (PDThese a)
  where
  ppure = phoistAcyclic $ plam $ \x -> pcon . PDThat $ pdcons # pdata x # pdnil

-- TODO: Semigroup, Monoid for Term s (PDThese a b)
-- TODO: PTraversable for PDThese a
-- TODO: PConstantDecl for PDThese, linking to plutus-tx

{- | Case analysis for 'PThese'.

 @since 1.0.0
-}
pthese ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s ((a :--> c) :--> (b :--> c) :--> (a :--> b :--> c) :--> PThese a b :--> c)
pthese = phoistAcyclic $
  plam $ \f g fg t -> unTermCont $ do
    t' <- pmatchC t
    pure $ case t' of
      PThis ta -> f # ta
      PThat tb -> g # tb
      PThese ta tb -> fg # ta # tb

{- | Case analysis for 'PDThese'.

 @since 1.0.0
-}
pdthese ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> c) :--> (b :--> c) :--> (a :--> b :--> c) :--> PDThese a b :--> c)
pdthese = phoistAcyclic $
  plam $ \f g fg t -> unTermCont $ do
    t' <- pmatchC t
    case t' of
      PDThis ta -> do
        ta' <- pletC . pfromData $ pfield @"_0" # ta
        pure $ f # ta'
      PDThat tb -> do
        tb' <- pletC . pfromData $ pfield @"_0" # tb
        pure $ g # tb'
      PDThese tab -> do
        ta' <- pletC . pfromData $ pfield @"_0" # tab
        tb' <- pletC . pfromData $ pfield @"_1" # tab
        pure $ fg # ta' # tb'

{- | Convert a 'PThese' into a 'PDThese', assuming 'PData' instances for both
 \'sides\'.

 @since 1.0.0
-}
toPDThese ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s (PThese a b :--> PDThese a b)
toPDThese = phoistAcyclic $
  plam $ \t -> unTermCont $ do
    t' <- pmatchC t
    pure $ case t' of
      PThis ta -> pcon . PDThis $ pdcons # pdata ta # pdnil
      PThat tb -> pcon . PDThat $ pdcons # pdata tb # pdnil
      PThese ta tb -> pcon . PDThese $ pdcons # pdata ta # (pdcons # pdata tb # pdnil)

{- | Convert a 'PDThese' into a 'PThese'.

 @since 1.0.0
-}
fromPDThese ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s (PDThese a b :--> PThese a b)
fromPDThese = phoistAcyclic $
  plam $ \t -> unTermCont $ do
    t' <- pmatchC t
    case t' of
      PDThis ta -> do
        ta' <- pletC . pfromData $ pfield @"_0" # ta
        pure . pcon . PThis $ ta'
      PDThat tb -> do
        tb' <- pletC . pfromData $ pfield @"_0" # tb
        pure . pcon . PThat $ tb'
      PDThese tab -> do
        ta' <- pletC . pfromData $ pfield @"_0" # tab
        tb' <- pletC . pfromData $ pfield @"_1" # tab
        pure . pcon . PThese ta' $ tb'
