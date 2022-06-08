{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Map (
    plookup,
    plookup',
    pmapFromList,
    pkeys,
    pupdate,
    pmap,
    punionWith,
    pkeysEqual,

    -- * PlutusTx Utilities.
    update,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Plutarch (
    PMatch (pmatch),
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    pto,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Api.V1.AssocMap (KeyGuarantees, PMap (PMap))
import Plutarch.Bool (PBool (PFalse), PEq ((#==)), POrd ((#<)), pif, pnot)
import Plutarch.Builtin (
    PAsData,
    PBuiltinList (PCons, PNil),
    PBuiltinMap,
    PBuiltinPair,
    PIsData,
    pdata,
    pfromData,
    pfstBuiltin,
    ppairDataBuiltin,
    psndBuiltin,
 )
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.List (pmapMaybe, pmsortBy)
import qualified Plutarch.Extra.List
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.List (pany, pconcat, pfilter, pfind, plength, plistEquals)
import qualified Plutarch.List
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Trace (ptraceError)
import qualified PlutusTx.AssocMap as AssocMap

-- | @since 1.0.0
plookup ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PMap keys k v :--> PMaybe v)
plookup = phoistAcyclic $
    plam $ \x m -> unTermCont $ do
        PMap kvs <- pmatchC m
        res <- pmatchC (pfind # (go # x) # kvs)
        pure $ case res of
            PNothing -> pcon PNothing
            PJust y -> pcon . PJust . pfromData $ psndBuiltin # y
  where
    go ::
        forall (s' :: S).
        Term s' (k :--> PBuiltinPair (PAsData k) (PAsData v) :--> PBool)
    go = phoistAcyclic $
        plam $ \x p -> unTermCont $ do
            k' <- pletC (pfromData $ pfstBuiltin # p)
            pure $ k' #== x

-- | @since 1.0.0
plookup' ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData v, PIsData k, PEq k) =>
    Term s (k :--> PMap keys k v :--> v)
plookup' = phoistAcyclic $
    plam $ \x m -> unTermCont $ do
        res <- pmatchC (plookup # x # m)
        pure $ case res of
            PNothing -> ptraceError "plookup': Could not find key."
            PJust v -> v

-- | @since 1.0.0
pmapFromList ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData k, PIsData v) =>
    [(Term s k, Term s v)] ->
    Term s (PMap keys k v)
pmapFromList = pcon . PMap . foldl' go (pcon PNil)
  where
    go ::
        Term s (PBuiltinMap k v) ->
        (Term s k, Term s v) ->
        Term s (PBuiltinMap k v)
    go acc (k, v) = unTermCont $ do
        k' <- pletC (pdata k)
        v' <- pletC (pdata v)
        p <- pletC (ppairDataBuiltin # k' # v')
        pure . pcon . PCons p $ acc

-- | @since 1.0.0
pkeys ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    Term s (PMap keys k v :--> PBuiltinList (PAsData k))
pkeys = phoistAcyclic $
    plam $ \m -> unTermCont $ do
        PMap kvs <- pmatchC m
        pure $ pfmap # pfstBuiltin # kvs

{- | / O(n) /. Update the value at a given key in a `PMap`, have the same functionalities as 'Data.Map.update'.

     @since 1.0.0
-}
pupdate ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData k, PIsData v) =>
    Term s ((v :--> PMaybe v) :--> k :--> PMap keys k v :--> PMap keys k v)
pupdate = phoistAcyclic $
    plam $ \f (pdata -> tk) (pto -> (ps :: Term _ (PBuiltinList _))) ->
        pcon $
            PMap $
                pmapMaybe
                    # plam
                        ( \kv ->
                            let k = pfstBuiltin # kv
                                v = pfromData $ psndBuiltin # kv
                             in pif
                                    (k #== tk)
                                    ( pmatch (f # v) $
                                        \case
                                            PJust uv -> pcon $ PJust $ ppairDataBuiltin # k # pdata uv
                                            _ -> pcon PNothing
                                    )
                                    (pcon $ PJust kv)
                        )
                    # ps

{- | / O(n) /. Map a function over all values in a 'PMap'.

     @since 1.0.0
-}
pmap ::
    forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData k, PIsData a, PIsData b) =>
    Term s ((a :--> b) :--> PMap keys k a :--> PMap keys k b)
pmap = phoistAcyclic $
    plam $ \f (pto -> (ps :: Term _ (PBuiltinList _))) ->
        pcon $
            PMap $
                Plutarch.List.pmap
                    # plam
                        ( \kv ->
                            let k = pfstBuiltin # kv
                                v = psndBuiltin # kv

                                nv = pdata $ f # pfromData v
                             in ppairDataBuiltin # k # nv
                        )
                    # ps

{- | Union two maps using a merge function on collisions.

    TODO(Emily): this function is kinda suspect. I feel like a lot of optimizations could be done here
    TODO: optimize this

    @since 1.0.0
-}
punionWith ::
    forall (k :: S -> Type) (v :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    PIsData v =>
    Term
        s
        ( (v :--> v :--> v)
            :--> PMap keys k v
            :--> PMap keys k v
            :--> PMap keys k v
        )
punionWith = phoistAcyclic $
    plam $ \f xs' ys' -> unTermCont $ do
        PMap xs <- pmatchC xs'
        PMap ys <- pmatchC ys'
        let ls =
                Plutarch.List.pmap
                    # plam
                        ( \p -> unTermCont $ do
                            pf <- pletC $ pfstBuiltin # p
                            pure $
                                pmatch (Plutarch.Extra.List.plookup # pf # ys) $ \case
                                    PJust v ->
                                        -- Data conversions here are silly, aren't they?
                                        ppairDataBuiltin # pf # pdata (f # pfromData (psndBuiltin # p) # pfromData v)
                                    PNothing -> p
                        )
                    # xs
            rs =
                pfilter
                    # plam
                        ( \p ->
                            pnot #$ pany # plam (\p' -> pfstBuiltin # p' #== pfstBuiltin # p) # xs
                        )
                    # ys
        pure $ pcon (PMap $ pconcat # ls # rs)

{- | True if both maps have exactly the same keys.
     Using @'#=='@ is not sufficient, because keys returned are not ordered.

    @since 1.0.0
-}
pkeysEqual ::
    forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (POrd k, PIsData k) =>
    Term s (PMap keys k a :--> PMap keys k b :--> PBool)
pkeysEqual = phoistAcyclic $
    plam $ \p q -> unTermCont $ do
        pks <- pletC $ pkeys # p
        qks <- pletC $ pkeys # q

        pure $
            pif
                (plength # pks #== plength # qks)
                ( unTermCont $ do
                    let comp = phoistAcyclic $ plam $ \(pfromData -> x) (pfromData -> y) -> x #< y
                        spks = pmsortBy # comp # pks
                        sqks = pmsortBy # comp # qks

                    pure $ plistEquals # spks # sqks
                )
                (pcon PFalse)

--------------------------------------------------------------------------------
-- Haskell level utilities.

{- | / O(n) /. The expression @'updateMap' f k v@ will update the value @x@ at key @k@.
    If @f x@ is Nothing, the key-value pair will be deleted from the map, otherwise the
     value will be updated.

     @since 1.0.0
-}
update :: Eq k => (v -> Maybe v) -> k -> AssocMap.Map k v -> AssocMap.Map k v
update f k =
    AssocMap.mapMaybeWithKey
        ( \k' v ->
            if k' == k
                then f v
                else Just v
        )
