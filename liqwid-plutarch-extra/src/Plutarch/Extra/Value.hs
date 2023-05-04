{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Value (
  -- * Creation
  passetClassDataValue,
  passetClassDataValueT,
  psingleValue,
  psingleValue',
  psingleValueT',
  pvalue,
  pvaluePositive,

  -- * Queries
  padaOf,
  passetClassValueOf',
  passetClassValueOf,
  passetClassValueOfT',
  passetClassValueOfT,
  passetClassDataValueOf,
  passetClassDataValueOfT,
  pmatchValueAssets,
  psplitValue,

  -- * Aggregation and elimination
  psymbolValueOf,
  psymbolValueOf',
  precValue,
  pelimValue,

  -- * Comparison
  pbyClassComparator,
  pbySymbolComparator,
  pbyClassComparator',

  -- * Miscellaneous
  AddGuarantees,
  phasOnlyOneTokenOfCurrencySymbol,
  phasOneTokenOfCurrencySymbol,
  phasOneTokenOfAssetClass,
  phasOneTokenOfAssetClassData,
  pcountNonZeroes,
) where

import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (Tagged))
import GHC.TypeLits (Symbol)
import Optics.Getter (A_Getter, view)
import Optics.Internal.Optic (Is)
import Optics.Label (LabelOptic')
import Plutarch.Api.V1 (
  AmountGuarantees (NonZero),
  PCurrencySymbol,
  PMap (PMap),
  PTokenName,
  PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value (padaSymbol)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  AmountGuarantees (NoGuarantees, Positive),
  KeyGuarantees (Sorted),
 )
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.DataRepr.Internal.Field (HRec (HCons, HNil), Labeled (Labeled))
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.AssetClass (
  AssetClass (AssetClass),
  PAssetClass (PAssetClass),
  PAssetClassData,
 )
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Functor (PFunctor (pfmap))
import Plutarch.Extra.IsData (PlutusTypeEnumData)
import Plutarch.Extra.List (pfromList, pfromSingleton, plookupAssoc, ptryElimSingle, ptryFromSingleton)
import Plutarch.Extra.Map (phandleMin)
import Plutarch.Extra.Maybe (pexpectJustC)
import Plutarch.Extra.Ord (PComparator, pfromOrdBy)
import Plutarch.Extra.Tagged (PTagged (PTagged))
import Plutarch.Extra.TermCont (pmatchC)
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)

--------------------------------------------------------------------------------

----------------------------------------
-- Value Creation

{- | Create a 'PValue' that only contains a specific amount of tokens, described
 by a 'PAssetClassData'.

 @since 3.10.0
-}
passetClassDataValue ::
  forall (s :: S).
  Term s (PAssetClassData :--> PInteger :--> PValue 'Sorted 'NonZero)
passetClassDataValue = phoistAcyclic $
  plam $ \ac i ->
    pif
      (i #== 0)
      (ptraceError "passetClassDataValue: given zero argument, expecting nonzero.")
      ( let cs = pfield @"symbol" # ac
            tn = pfield @"name" # ac
         in Value.psingleton # pfromData cs # pfromData tn # i
      )

{- | Tagged version of `passetClassDataValue`.

 @since 3.14.1
-}
passetClassDataValueT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Term
    s
    ( PTagged unit PAssetClassData
        :--> PTagged unit PInteger
        :--> PValue 'Sorted 'NonZero
    )
passetClassDataValueT = phoistAcyclic $
  plam $ \ac i ->
    passetClassDataValue # (pextract # ac) # (pextract # i)

{- | Helper to construct the \'inner mapping\' of a 'PValue'.

 @since 3.9.0
-}
psingleValue ::
  forall (key :: KeyGuarantees) (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap key PTokenName PInteger))
    )
psingleValue = phoistAcyclic $
  plam $ \sym tk q ->
    ppairDataBuiltin
      # sym
      # pdata (pcon $ PMap $ pfromList [ppairDataBuiltin # tk # pdata q])

{- | As 'psingleValue', but using a Haskell-level 'AssetClass'.

 @since 3.10.0
-}
psingleValue' ::
  forall (keys :: KeyGuarantees) (a :: Type) (k :: Type) (s :: S).
  ( Is k A_Getter
  , LabelOptic' "symbol" k a CurrencySymbol
  , LabelOptic' "name" k a TokenName
  ) =>
  a ->
  Term
    s
    ( PInteger
        :--> PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap keys PTokenName PInteger))
    )
psingleValue' ac =
  phoistAcyclic $
    psingleValue
      # pconstantData (view #symbol ac)
      # pconstantData (view #name ac)

{- | Tagged version of `psingleValue'`.

 @since 3.14.1
-}
psingleValueT' ::
  forall {k :: Type} (keys :: KeyGuarantees) (unit :: k) (s :: S).
  Tagged unit AssetClass ->
  Term
    s
    ( PTagged unit PInteger
        :--> PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap keys PTokenName PInteger))
    )
psingleValueT' (Tagged (AssetClass sym tk)) =
  phoistAcyclic $
    plam $ \q ->
      psingleValue # pconstantData sym # pconstantData tk #$ pextract # q

{- | Construct a 'PValue' from its underlying representation.
 There are "NoGuarantees" on the amounts.

 @since 3.10.0
-}
pvalue ::
  forall (k :: KeyGuarantees) (s :: S).
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
        :--> PAsData (PValue k 'NoGuarantees)
    )
pvalue = phoistAcyclic $ plam $ pdata . pcon . PValue . pcon . PMap

{- | Construct a 'PValue' from its underlying representation.
 The amounts are guaranteed to be "Positive", so this is suitable for
 construction of "PTxOut"s.

 @since 3.9.0
-}
pvaluePositive ::
  forall (k :: KeyGuarantees) (s :: S).
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
        :--> PAsData (PValue k 'Positive)
    )
pvaluePositive =
  phoistAcyclic $
    plam $ \x ->
      pdata $
        Value.passertPositive
          # (pcon . PValue . pcon . PMap $ x)

----------------------------------------
-- Value Querying, Extraction, and Matching

{- | Get the amount of ada of a 'PValue'.

   @since 3.9.0
-}
padaOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PValue keys amounts :--> PInteger)
padaOf = phoistAcyclic $
  plam $ \v ->
    Value.pvalueOf # v # pconstant "" # pconstant ""

{- | As 'passetClassValueOf', but using a Haskell-level 'AssetClass'.

 @since 3.9.0
-}
passetClassValueOf' ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (a :: Type)
    (k :: Type)
    (s :: S).
  ( Is k A_Getter
  , LabelOptic' "symbol" k a CurrencySymbol
  , LabelOptic' "name" k a TokenName
  ) =>
  a ->
  Term s (PValue keys amounts :--> PInteger)
passetClassValueOf' ac =
  phoistAcyclic $
    plam $ \value ->
      Value.pvalueOf # value # pconstant (view #symbol ac) # pconstant (view #name ac)

{- | Tagged version of `passetClassValueOf'`.

 @since 3.14.1
-}
passetClassValueOfT' ::
  forall {k :: Type} (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (unit :: k) (s :: S).
  Tagged unit AssetClass ->
  Term s (PValue keys amounts :--> PTagged unit PInteger)
passetClassValueOfT' (Tagged (AssetClass sym token)) =
  phoistAcyclic $
    plam $ \value ->
      ppure #$ Value.pvalueOf # value # pconstant sym # pconstant token

{- | Given a 'PAssetClass' and a 'PValue', look up the amount corresponding to
 that 'PAssetClass'.

 @since 3.9.0
-}
passetClassValueOf ::
  forall
    (key :: KeyGuarantees)
    (amount :: AmountGuarantees)
    (s :: S).
  Term s (PAssetClass :--> PValue key amount :--> PInteger)
passetClassValueOf = phoistAcyclic $
  plam $ \cls val -> pmatch cls $ \(PAssetClass sym tk) ->
    precList (findValue sym tk) (const 0) #$ pto $ pto val

{- | Tagged version of `passetClassValueOf`.

 @since 3.9.0
-}
passetClassValueOfT ::
  forall
    {k :: Type}
    (key :: KeyGuarantees)
    (amount :: AmountGuarantees)
    (unit :: k)
    (s :: S).
  Term s (PTagged unit PAssetClass :--> PValue key amount :--> PTagged unit PInteger)
passetClassValueOfT = phoistAcyclic $
  plam $ \cls val -> pmatch (pextract # cls) $ \(PAssetClass sym tk) ->
    ppure #$ precList (findValue sym tk) (const 0) #$ pto $ pto val

{- | Given a 'PAssetClassData' and a 'PValue', look up the amount corresponding
 to that 'PAssetClassData'.

 @since 3.21.4
-}
passetClassDataValueOf ::
  forall
    (key :: KeyGuarantees)
    (amount :: AmountGuarantees)
    (s :: S).
  Term s (PAssetClassData :--> PValue key amount :--> PInteger)
passetClassDataValueOf = phoistAcyclic $
  plam $ \cls val ->
    let cs = pfield @"symbol" # cls
        tn = pfield @"name" # cls
     in precList (findValue cs tn) (const 0) #$ pto $ pto val

{- | Tagged version of `passetClassDataValueOf`.

 @since 3.21.4
-}
passetClassDataValueOfT ::
  forall
    {k :: Type}
    (key :: KeyGuarantees)
    (amount :: AmountGuarantees)
    (unit :: k)
    (s :: S).
  Term s (PTagged unit PAssetClassData :--> PValue key amount :--> PTagged unit PInteger)
passetClassDataValueOfT = phoistAcyclic $
  plam $ \cls val ->
    ppure #$ passetClassDataValueOf # (pextract # cls) # val

{- | Extracts the amount given by the 'PAssetClass' from (the internal
 representation of) a 'PValue'.

 Intuitively, this acts as a \'pattern match\' on 'PValue'. You don't have to
 provide every asset class, only the ones you are interested in.

 = Example

 > example :: forall (s :: S) .
 >    Term s (PBuiltinList (
 >      PBuiltinPair (PAsData PCurrencySymbol)
 >                   (PAsData (PMap 'Sorted PTokenName PInteger))
 >      ) -> TermConst s ()
 > example rep = do
 >    rec <- matchValueAssets @'['("ada", AssetClass "Ada")]
 >              rep
 >              (HCons (Labeled adaClass) HNil)
 >    -- Or using operators
 >    rec2 <- matchValueAssets rep $ (Proxy @"ada" .|== adaClass) HNIl
 >    let adaFromRep = rec.ada

 @since 3.9.0
-}
pmatchValueAssets ::
  forall (input :: [(Symbol, Type)]) (s :: S).
  ( MatchValueAssetReferences input s
  , HRecToList input AssetClass
  ) =>
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap 'Sorted PTokenName PInteger))
        )
    ) ->
  HRec input ->
  TermCont s (HRec (OutputMatchValueAssets input s))
pmatchValueAssets pvaluemap inputs = do
  -- nub reduces case Underlying being Ada also
  let sortedInputs = sort . nub . hrecToList $ inputs
  -- perform actuall pattern match and save references to Haskell's Map
  matchedMap <- unsafeMatchValueAssetsInternal pvaluemap sortedInputs
  -- reconstruct HRec with references saved on the matchedMap
  pure $ matchValueAssetReferences @input matchedMap inputs

{- |
  Gets the first entry in the first item of the 'PValue' mapping & returns the
  rest of the 'PValue'.

  Fails if the 'PValue' is empty. In cases where we know that a 'PValue' contains
  Ada, such as in a 'PScriptContext', then this will function will split the
  Ada value - since the Ada entry comes first.

  NOTE: All properly normalized values will contain an Ada entry, even if
  that entry is 0.

  @since 3.9.0
-}
psplitValue ::
  forall (v :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'Sorted v
        :--> PPair
              (PAsData PInteger)
              (PValue 'Sorted v)
    )
psplitValue = phoistAcyclic $
  plam $ \v ->
    let vList = pto $ pto v
     in pcon $
          PPair
            ( psndBuiltin #$ phead #$ pto $
                pto $
                  pfromData
                    (psndBuiltin # (phead # vList))
            )
            (pcon $ PValue $ pcon $ PMap $ ptail # vList)

----------------------------------------
-- Value Aggregation and Elimination

{- | Get the sum of all values belonging to a particular CurrencySymbol.

   @since 1.1.0
-}
psymbolValueOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
psymbolValueOf =
  phoistAcyclic $
    plam $ \sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <- pexpectJustC 0 (plookupAssoc # pfstBuiltin # psndBuiltin # pdata sym # value)
      PMap m <- pmatchC (pfromData m')
      pure $ pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m

{- | Eliminator for a sorted 'PValue'. The function argument will receive:

 * The smallest 'PCurrencySymbol' for which we have a mapping;
 * The smallest 'PTokenName' for which the corresponding \'inner map\' has a
 mapping;
 * The 'PInteger' associated with the above 'PTokenName'; and
 * The \'rest\'.

 If the \'inner map\' corresponding to the 'PCurrencySymbol' above contains
 only a single mapping, the \'rest\' will not contain a mapping for that
 'PCurrencySymbol'; otherwise, it will contain the rest of the \'inner map\'.

 = Note

 The \'nil case\' will be invoked in two situations:

 * When the 'PValue' has no entries; and
 * When the \'inner map\' corresponding to the first 'PCurrencySymbol' key has
 no entries.

 @since 3.9.0
-}
pelimValue ::
  forall (amounts :: AmountGuarantees) (r :: S -> Type) (s :: S).
  ( Term s PCurrencySymbol ->
    Term s PTokenName ->
    Term s PInteger ->
    Term s (PValue 'Sorted amounts) ->
    Term s r
  ) ->
  Term s r ->
  Term s (PValue 'Sorted amounts) ->
  Term s r
pelimValue whenCons whenNil xs = phandleMin (pto xs) whenNil $ \k v kvs ->
  phandleMin v whenNil $ \vk vv rest ->
    whenCons k vk vv . pcon . PValue $
      pif
        (AssocMap.pnull # rest)
        kvs
        (AssocMap.pinsert # k # rest # kvs)

{- | Eliminator for the inner type of a 'PValue'.

 @since 3.9.0
-}
precValue ::
  forall r (k :: KeyGuarantees) s.
  ( Term
      s
      ( PBuiltinList
          ( PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap k PTokenName PInteger))
          )
          :--> r
      ) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PInteger) ->
    Term
      s
      ( PBuiltinList
          ( PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap k PTokenName PInteger))
          )
      ) ->
    Term s r
  ) ->
  ( Term
      s
      ( PBuiltinList
          ( PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (PMap k PTokenName PInteger))
          )
          :--> r
      ) ->
    Term s r
  ) ->
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
        :--> r
    )
precValue mcons =
  precList
    ( \self symbolMap symbolMaps ->
        pelimList
          ( \tokenMap tokenMaps ->
              mcons
                self
                (pfstBuiltin # symbolMap)
                (pfstBuiltin # tokenMap)
                (psndBuiltin # tokenMap)
                ( pif
                    (pnull # tokenMaps)
                    symbolMaps
                    ( pcons
                        # ( ppairDataBuiltin
                              # (pfstBuiltin # symbolMap)
                              # pdata (pcon (PMap tokenMaps))
                          )
                        # symbolMaps
                    )
                )
          )
          (self # symbolMaps)
          (pto $ pfromData $ psndBuiltin # symbolMap)
    )

----------------------------------------
-- Value Comparison

{- | Compare only on the basis of a particular 'PCurrencySymbol' and
 'PTokenName' entry.

 @since 3.9.0
-}
pbyClassComparator ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PComparator (PValue keys amounts))
pbyClassComparator = phoistAcyclic $
  plam $ \cs tn ->
    pfromOrdBy # plam (\pval -> Value.pvalueOf # pval # cs # tn)

{- | Compare only the entries corresponding to a particular 'PCurrencySymbol'.

 @since 3.9.0
-}
pbySymbolComparator ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PComparator (PValue keys amounts))
pbySymbolComparator = phoistAcyclic $
  plam $ \cs ->
    pfromOrdBy # plam (psymbolValueOf # cs #)

{- | As 'pbyClassComparator', but using a Haskell-level 'AssetClass' instead.

 @since 3.9.0
-}
pbyClassComparator' ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  AssetClass ->
  Term s (PComparator (PValue keys amounts))
pbyClassComparator' ac = pfromOrdBy # plam (passetClassValueOf' ac #)

----------------------------------------
-- Miscellaneous
-- TODO: Peter, 2022-09-21: There's a lot of typeclasses and instances that I
-- haven't looked at yet. I don't know how many of these belong in this module vs
-- should be factored out.

{- | Compute the guarantees known after adding two 'PValue's.

   @since 1.1.0
-}
type family AddGuarantees (a :: AmountGuarantees) (b :: AmountGuarantees) where
  AddGuarantees 'Positive 'Positive = 'Positive
  AddGuarantees _ _ = 'NoGuarantees

-- Used internally only in 'phasOnlyOneTokenOfCurrencySymbol'
data PState (s :: S)
  = PInitial
  | PFound
  | PFailed
  deriving stock
    ( Generic
    , Enum
    , Bounded
    )
  deriving anyclass
    ( PlutusType
    )

instance DerivePlutusType PState where
  type DPTStrat _ = PlutusTypeEnumData

{- | Returns 'PTrue' if the entire argument 'PValue' contains /exactly/ one
     token of the argument 'PCurrencySymbol' (and contains no other assets).

     This implementation makes a special case for ADA, where it allows
     zero-ada entries in the given 'PValue'.

     @since 3.21.0
-}
phasOnlyOneTokenOfCurrencySymbol ::
  forall (kg :: KeyGuarantees) (ag :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue kg ag :--> PBool)
phasOnlyOneTokenOfCurrencySymbol =
  {- Implementation notes:

      This is implemented using a state machine with three states: 'PInitial', 'PFound', 'PFailed'.

      See this comment for the state transition graph:
      https://gist.github.com/chfanghr/c3ef2f0ed1561e7b17dd11c1df609479?permalink_comment_id=4443620#gistcomment-4443620

      This implementation makes a special case for ADA, where it allows zero-ada entries in the 'PValue'.
  -}
  plam $
    \cs
     ( (pto . pto) ->
        l
      ) ->
        let isZeroAdaEntry ::
              Term
                s
                ( PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap kg PTokenName PInteger))
                    :--> PBool
                )
            isZeroAdaEntry = plam $ \pair ->
              let cs' = pfromData $ pfstBuiltin # pair
                  isAda = ptraceIfFalse "Not ada" $ cs' #== padaSymbol

                  tnMap = pfromData $ psndBuiltin # pair
                  count = pfromData $ psndBuiltin # (ptryFromSingleton # pto tnMap)
                  zeroAda = ptraceIfFalse "Non zero ada" $ count #== 0
               in isAda #&& zeroAda

            isNonAdaEntryValid ::
              Term
                s
                ( PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap kg PTokenName PInteger))
                    :--> PBool
                )
            isNonAdaEntryValid = plam $ \pair ->
              let cs' = pfromData $ pfstBuiltin # pair
                  validCs = ptraceIfFalse "Unknown symbol" $ cs' #== cs

                  tnMap = pfromData $ psndBuiltin # pair
                  validTnMap = ptraceIfFalse "More than one token names or tokens" $
                    pmatch (pfromSingleton # pto tnMap) $ \case
                      PNothing -> pcon PFalse
                      PJust ((pfromData . (psndBuiltin #)) -> tokenCount) ->
                        tokenCount #== 1
               in validCs #&& validTnMap

            go ::
              Term
                s
                ( ( PState
                      :--> PBuiltinList
                            ( PBuiltinPair
                                (PAsData PCurrencySymbol)
                                (PAsData (PMap kg PTokenName PInteger))
                            )
                      :--> PState
                  )
                    :--> PState
                    :--> PBuiltinList
                          ( PBuiltinPair
                              (PAsData PCurrencySymbol)
                              (PAsData (PMap kg PTokenName PInteger))
                          )
                    :--> PState
                )
            go =
              plam $ \self lastState ->
                pelimList
                  ( \x xs -> pif
                      (isZeroAdaEntry # x)
                      (self # lastState # xs)
                      $ pmatch lastState
                      $ \case
                        PInitial ->
                          pif
                            (isNonAdaEntryValid # x)
                            (self # pcon PFound # xs)
                            (pcon PFailed)
                        PFound -> pcon PFailed
                        PFailed -> ptraceError "unreachable"
                  )
                  ( pmatch lastState $ \case
                      PFound -> lastState
                      _ -> pcon PFailed
                  )
         in pmatch (pfix # go # pcon PInitial # l) $
              \case
                PFound -> pcon PTrue
                _ -> pcon PFalse

{- | Returns the count of non-zero currency symbols in a 'PValue'.

     So, for a value of the following shape:

     @
       [("", [("", 0)]), ("feed", [("foo", 7)]), ("deed", [("bar", 1)])]
     @

     We get the result @2@.

     @since 3.21.0
-}
pcountNonZeroes :: forall (kg :: KeyGuarantees) (ag :: AmountGuarantees) (s :: S). Term s (PValue kg ag :--> PInteger)
pcountNonZeroes = plam $ \value ->
  let
    nonZero :: Term s ((PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap kg PTokenName PInteger))) :--> PBool)
    nonZero = plam $ \currencySymbolPair ->
      plet (pto $ pfromData $ psndBuiltin # currencySymbolPair) $ \tokens ->
        pall
          # plam (\tokenNamePair -> pnot #$ (pfromData $ psndBuiltin # tokenNamePair) #== 0)
          # tokens
            #&& pnot
          # (pnull # tokens)
   in
    plength #$ pfilter # nonZero #$ pto $ pto value

{- | Returns 'PTrue' if the argument 'PValue' contains /exactly/
  one token of the argument 'PAssetClassData'.

 Note: unlike `phasOnlyOneTokenOfCurrencySymbol` this may
 still return 'PTrue' if there are other assets in the 'PValue'.

 @since 3.21.4
-}
phasOneTokenOfAssetClassData ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PAssetClassData :--> PValue keys amounts :--> PBool)
phasOneTokenOfAssetClassData = phoistAcyclic $
  plam $ \cls v ->
    passetClassDataValueOf # cls # v #== 1

{- | Returns 'PTrue' if the argument 'PValue' contains /exactly/
  one token of the argument 'PAssetClass'.

 Note: unlike `phasOnlyOneTokenOfCurrencySymbol` this may
 still return 'PTrue' if there are other assets in the 'PValue'.

 @since 3.9.1
-}
phasOneTokenOfAssetClass ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PAssetClass :--> PValue keys amounts :--> PBool)
phasOneTokenOfAssetClass = phoistAcyclic $
  plam $ \cls v ->
    passetClassValueOf # cls # v #== 1

{- | Returns 'PTrue' if the argument 'PValue' contains /exactly/ one
 token of the argument 'PCurrencySymbol'.

 Note: unlike `phasOnlyOneTokenOfCurrencySymbol` this may
 still return 'PTrue' if there are other assets in the 'PValue'.

 @since 3.9.1
-}
phasOneTokenOfCurrencySymbol ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PBool)
phasOneTokenOfCurrencySymbol = phoistAcyclic $
  plam $ \cs vs ->
    psymbolValueOf # cs # vs #== 1

-- Helpers

-- Cons case for QTokenValue, with recursive fixpoint
findValue ::
  forall (k :: KeyGuarantees) (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
        :--> PInteger
    ) ->
  Term
    s
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ) ->
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
    ) ->
  Term s PInteger
findValue sym tk self x xs = plet x $ \pair ->
  pif
    (pforgetData (pfstBuiltin # pair) #== pforgetData sym)
    (ptryElimSingle go . pto . pfromData $ psndBuiltin # pair)
    (self # xs)
  where
    go ::
      Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
      Term s PInteger
    go kv =
      pif
        (pfstBuiltin # kv #== tk)
        (pfromData $ psndBuiltin # kv)
        (ptraceError "findValue: Unexpectedly missing result.")

unsafeMatchValueAssetsInternal ::
  forall (k :: KeyGuarantees) (s :: S).
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
    ) ->
  [AssetClass] ->
  TermCont s (Map AssetClass (Term s (PAsData PInteger)))
unsafeMatchValueAssetsInternal _ [] = pure Map.empty
unsafeMatchValueAssetsInternal
  inputpvalue
  (sa@someAsset : rest) = do
    (PPair pint newValueMap) <-
      TermCont . pmatch $
        matchOrTryRec someAsset
          # inputpvalue
    remaining <- unsafeMatchValueAssetsInternal newValueMap rest
    pure $ Map.insert sa pint remaining

-- This function does not check if keys are duplicated or are in different
-- order
class HRecToList (xs :: [(Symbol, Type)]) (x :: Type) where
  hrecToList :: HRec xs -> [x]

instance HRecToList '[] (x :: Type) where
  hrecToList _ = []

-- Converts type level lists of tagged assets back to dynamic-typed assets
instance
  forall (rest :: [(Symbol, Type)]) (name :: Symbol) (unit :: Symbol).
  HRecToList rest AssetClass =>
  HRecToList
    ('(name, Tagged unit AssetClass) ': rest)
    AssetClass
  where
  hrecToList (HCons (Labeled (Tagged x)) xs) = x : hrecToList xs

-- | Associates given Symbol of AssetClass to PTagged tag PInteger
type family OutputMatchValueAssets (ps :: [(Symbol, Type)]) (s :: S) where
  OutputMatchValueAssets '[] _ = '[]
  OutputMatchValueAssets ('(name, Tagged unit AssetClass) ': rest) s =
    '( name
     , Term
        s
        (PAsData (PTagged unit PInteger))
     )
      ': OutputMatchValueAssets rest s

class MatchValueAssetReferences (input :: [(Symbol, Type)]) (s :: S) where
  matchValueAssetReferences ::
    Map AssetClass (Term s (PAsData PInteger)) ->
    HRec input ->
    HRec (OutputMatchValueAssets input s)

instance MatchValueAssetReferences '[] (s :: S) where
  matchValueAssetReferences _ _ = HNil

instance
  forall
    (name :: Symbol)
    (classSymbol :: Symbol)
    (is :: [(Symbol, Type)])
    (s :: S).
  MatchValueAssetReferences is s =>
  MatchValueAssetReferences
    ('(name, Tagged classSymbol AssetClass) ': is)
    s
  where
  matchValueAssetReferences valueMap (HCons (Labeled (Tagged cls)) rest) =
    HCons
      ( Labeled @name
          ( pdata $
              pcon $
                PTagged $
                  pfromData (valueMap Map.! cls)
          )
      )
      (matchValueAssetReferences valueMap rest)

matchOrTryRec ::
  forall (k :: KeyGuarantees) (s :: S).
  AssetClass ->
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        )
        :--> PPair
              (PAsData PInteger)
              ( PBuiltinList
                  ( PBuiltinPair
                      (PAsData PCurrencySymbol)
                      (PAsData (PMap k PTokenName PInteger))
                  )
              )
    )
matchOrTryRec requiredAsset = phoistAcyclic $
  plam $ \inputpvalue -> plet (pcon $ PPair (pdata 0) inputpvalue) $ \def ->
    precValue
      ( \self pcurrencySymbol pTokenName pint rest ->
          pif
            ( pconstantData (view #symbol requiredAsset)
                #== pcurrencySymbol
                #&& pconstantData (view #name requiredAsset)
                #== pTokenName
            )
            -- assetClass found - return its quantity and tail of PValueMap
            (pcon $ PPair pint rest)
            -- assetClass not found - skipping current position
            (self # rest)
      )
      (const def)
      # inputpvalue

{- | Get the negative and positive amount of a particular 'CurrencySymbol', and
     return nothing if it doesn't exist in the value.

     @since 3.14.1
-}
psymbolValueOf' ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PValue keys amounts
        :--> PMaybe
              ( PPair
                  -- Positive amount
                  PInteger
                  -- Negative amount
                  PInteger
              )
    )
psymbolValueOf' = phoistAcyclic $
  plam $ \sym value ->
    let tnMap = plookup # sym # pto value
        f =
          plam $
            ( pfoldr
                # plam
                  ( \x r ->
                      let q = pfromData $ psndBuiltin # x
                       in pmatch r $ \(PPair p n) ->
                            pif
                              (0 #< q)
                              (pcon $ PPair (p + q) n)
                              (pcon $ PPair p (n + q))
                  )
                # pcon (PPair 0 0)
                #
            )
              . pto
     in pfmap # f # tnMap
