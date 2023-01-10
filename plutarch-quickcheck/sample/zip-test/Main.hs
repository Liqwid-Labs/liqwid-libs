{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental
 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Builtin ()
import Plutarch.List (puncons)
import Plutarch.Prelude (
  PBool,
  PBuiltinList,
  PElemConstraint,
  PListLike,
  PMaybe (PJust, PNothing),
  PPair (PPair),
  S,
  Term,
  Type,
  pcons,
  pconstant,
  pfix,
  phoistAcyclic,
  pif,
  plam,
  plength,
  plet,
  pmatch,
  pnil,
  tcont,
  unTermCont,
  (#),
  (#$),
  (#&&),
  (#<),
  (#==),
  (:-->),
 )
import Plutarch.Test.QuickCheck (PA, fromPFun, parbitrary)

import Test.QuickCheck (
  arbitrary,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Property, QuickCheckTests, forAll, testProperty)

pZip ::
  forall
    (l3 :: (S -> Type) -> S -> Type)
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (l1 :: (S -> Type) -> S -> Type)
    (l2 :: (S -> Type) -> S -> Type)
    (s :: S).
  ( PElemConstraint l1 a
  , PListLike l1
  , PListLike l2
  , PElemConstraint l2 b
  , PListLike l3
  , PElemConstraint l3 c
  ) =>
  Term s ((a :--> b :--> c) :--> l1 a :--> l2 b :--> l3 c)
pZip = phoistAcyclic $
  pfix #$ plam $ \self f txs tys -> unTermCont $ do
    tleft <- tcont (pmatch (puncons # txs))
    tright <- tcont (pmatch (puncons # tys))
    case (tleft, tright) of
      (PNothing, PNothing) -> pure pnil
      (PNothing, _) -> pure pnil
      (_, PNothing) -> pure pnil
      (PJust t, PJust t') -> do
        PPair tx txs' <- tcont (pmatch t)
        PPair ty tys' <- tcont (pmatch t')
        pure $ pcons # (f # tx # ty) # (self # f # txs' # tys')

zipLengthProperty :: Property
zipLengthProperty = forAll parbitrary $ fromPFun go
  where
    go :: Term s (PBuiltinList PA :--> PBool)
    go = plam $ \l ->
      (plength # l) #== (plength #$ pZip @PBuiltinList # plam (+) # l # l)

zipUnevenProperty :: Property
zipUnevenProperty = forAll arbitrary $ fromPFun go
  where
    go :: Term s (PBuiltinList PA :--> PBuiltinList PA :--> PBool)
    go = plam $ \l1 l2 -> unTermCont $ do
      l1' <- tcont $ plet $ plength # l1
      l2' <- tcont $ plet $ plength # l2
      zl <- tcont $ plet $ plength #$ pZip @PBuiltinList # plam (+) # l1 # l2
      pure $ pif (l1' #< l2') (zl #== l1') (zl #== l2')

zipContentProperty :: Property
zipContentProperty = forAll arbitrary $ fromPFun go
  where
    check ::
      forall {s :: S}.
      Term
        s
        ( (PA :--> PA :--> PA)
            :--> PBuiltinList PA
            :--> PBuiltinList PA
            :--> PBuiltinList PA
            :--> PBool
        )
    check = plam $ \f ->
      pfix #$ plam $ \self l1 l2 zl -> unTermCont $ do
        t1 <- tcont $ pmatch $ puncons # l1
        t2 <- tcont $ pmatch $ puncons # l2
        tz <- tcont $ pmatch $ puncons # zl
        case (t1, t2, tz) of
          (PJust a, PJust b, PJust c) -> do
            PPair ta tas <- tcont $ pmatch a
            PPair tb tbs <- tcont $ pmatch b
            PPair tc tcs <- tcont $ pmatch c
            pure $ (f # ta # tb) #== tc #&& (self # tas # tbs # tcs)
          _ -> pure $ pconstant True

    go ::
      forall {s :: S}.
      Term
        s
        ( PBuiltinList PA
            :--> PBuiltinList PA
            :--> PBool
        )
    go = plam $ \l1 l2 -> unTermCont $ do
      zl <- tcont $ plet $ pZip @PBuiltinList # plam (+) # l1 # l2
      pure $ check # plam (+) # l1 # l2 # zl

main :: IO ()
main = do
  -- This will fix some problems regarding text encoding.
  setLocaleEncoding utf8
  defaultMain . adjustOption go $
    testGroup
      ""
      [ testProperty "Length should be eqaul" zipLengthProperty
      , testProperty
          "zipped list should have length of shorter list"
          zipUnevenProperty
      , testProperty "zipped list has correct values" zipContentProperty
      ]
  where
    -- 100 tests is way too small for a property to reliably find a counterexample,
    -- it is recommended to use at least 10,000. However, more is better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
