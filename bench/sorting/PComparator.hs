module PComparator (
  PComparator1,
  pfromOrd1,
  pcompareBy1,
  pequateBy1,
  pleqBy1,
  plessThanBy1,
  pgeqBy1,
  pgreaterThanBy1,
  PComparator2,
  pfromOrd2,
  pcompareBy2,
  pequateBy2,
  pleqBy2,
  plessThanBy2,
  pgeqBy2,
  pgreaterThanBy2,
  PComparator3,
  pfromOrd3,
  pcompareBy3,
  pequateBy3,
  pleqBy3,
  plessThanBy3,
  pgeqBy3,
  pgreaterThanBy3,
) where

import Plutarch.Extra.Ord (POrdering (PEQ, PGT, PLT))

-- Using a unified function.
newtype PComparator1 (a :: S -> Type) (s :: S)
  = PComparator1 (Term s (a :--> a :--> POrdering))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PComparator1 a) where
  type DPTStrat _ = PlutusTypeNewtype

pfromOrd1 ::
  forall (a :: S -> Type) (s :: S).
  (POrd a) =>
  Term s (PComparator1 a)
pfromOrd1 = pcon . PComparator1 $
  phoistAcyclic $
    plam $ \x y ->
      pif (x #== y) (pcon PEQ) (pif (x #< y) (pcon PLT) (pcon PGT))

pcompareBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> POrdering)
pcompareBy1 = phoistAcyclic $ plam $ \cmp x y -> pto cmp # x # y

pequateBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> PBool)
pequateBy1 = phoistAcyclic $
  plam $ \cmp x y ->
    pto cmp # x # y #== pcon PEQ

pleqBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> PBool)
pleqBy1 = phoistAcyclic $
  plam $ \cmp x y ->
    pto cmp # x # y #< pcon PGT

plessThanBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> PBool)
plessThanBy1 = phoistAcyclic $
  plam $ \cmp x y ->
    pto cmp # x # y #== pcon PLT

pgeqBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> PBool)
pgeqBy1 = phoistAcyclic $
  plam $ \cmp x y ->
    pcon PLT #< pto cmp # x # y

pgreaterThanBy1 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator1 a :--> a :--> a :--> PBool)
pgreaterThanBy1 = phoistAcyclic $
  plam $ \cmp x y ->
    pto cmp # x # y #== pcon PGT

-- Using EQ + LE.
data PComparator2 (a :: S -> Type) (s :: S) = PComparator2
  { pcomparator2Eq :: Term s (a :--> a :--> PBool)
  , pcomparator2Le :: Term s (a :--> a :--> PBool)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PComparator2 a) where
  type DPTStrat _ = PlutusTypeScott

pfromOrd2 ::
  forall (a :: S -> Type) (s :: S).
  (POrd a) =>
  Term s (PComparator2 a)
pfromOrd2 =
  pcon . PComparator2 (phoistAcyclic $ plam (#==)) $
    phoistAcyclic $
      plam (#<=)

pcompareBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> POrdering)
pcompareBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 peq ple) ->
      pif
        (ple # x # y)
        (pif (peq # x # y) (pcon PEQ) (pcon PLT))
        (pcon PGT)

pequateBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> PBool)
pequateBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 peq _) ->
      peq # x # y

pleqBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> PBool)
pleqBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 _ ple) ->
      ple # x # y

plessThanBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> PBool)
plessThanBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 peq ple) ->
      (ple # x # y) #&& (pnot #$ peq # x # y)

pgeqBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> PBool)
pgeqBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 peq ple) ->
      (peq # x # y) #|| (pnot #$ ple # x # y)

pgreaterThanBy2 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator2 a :--> a :--> a :--> PBool)
pgreaterThanBy2 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator2 _ ple) ->
      pnot #$ ple # x # y

-- Cart around the entire dictionary why don't we.
data PComparator3 (a :: S -> Type) (s :: S) = PComparator3
  { pcomparator3Eq :: Term s (a :--> a :--> PBool)
  , pcomparator3Le :: Term s (a :--> a :--> PBool)
  , pcomparator3Lt :: Term s (a :--> a :--> PBool)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PComparator3 a) where
  type DPTStrat _ = PlutusTypeScott

pfromOrd3 ::
  forall (a :: S -> Type) (s :: S).
  (POrd a) =>
  Term s (PComparator3 a)
pfromOrd3 =
  pcon
    . PComparator3
      (phoistAcyclic $ plam (#==))
      (phoistAcyclic $ plam (#<=))
    $ phoistAcyclic
    $ plam (#<)

pcompareBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> POrdering)
pcompareBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 peq ple _) ->
      pif
        (ple # x # y)
        (pif (peq # x # y) (pcon PEQ) (pcon PLT))
        (pcon PGT)

pequateBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> PBool)
pequateBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 peq _ _) ->
      peq # x # y

pleqBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> PBool)
pleqBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 _ ple _) ->
      ple # x # y

plessThanBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> PBool)
plessThanBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 _ _ plt) ->
      plt # x # y

pgeqBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> PBool)
pgeqBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 _ _ plt) ->
      pnot #$ plt # x # y

pgreaterThanBy3 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PComparator3 a :--> a :--> a :--> PBool)
pgreaterThanBy3 = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator3 _ ple _) ->
      pnot #$ ple # x # y
