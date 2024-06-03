-- Whole module is just orphans
{-# OPTIONS_GHC -Wno-orphans #-}

{- | A collection of QuickCheck instances. This doesn't export any identifiers,
 so should be imported with an empty import list.
-}
module Plutarch.Test.QuickCheck.Instances () where

import Data.ByteString (ByteString)
import Data.Char (chr, ord)
import Data.Kind (Type)
import Data.Word (Word64, Word8)
import GHC.Exts (coerce, fromList, fromListN, toList)
import Plutarch.Test.QuickCheck.Modifiers (GenValue (GenValue))
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import PlutusLedgerApi.V2 (
  Address (Address, addressCredential, addressStakingCredential),
  BuiltinByteString,
  BuiltinData,
  Credential (PubKeyCredential, ScriptCredential),
  Data (B, Constr, I, List, Map),
  Datum (Datum),
  DatumHash (DatumHash),
  LedgerBytes (LedgerBytes),
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  ScriptHash (ScriptHash),
  StakingCredential (StakingHash, StakingPtr),
  TokenName (TokenName),
  TxId (TxId),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
  TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
  builtinDataToData,
  dataToBuiltinData,
  fromBuiltin,
  toBuiltin,
 )
import Test.QuickCheck (
  ASCIIString (ASCIIString),
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonNegative (NonNegative),
  Positive (Positive),
  functionMap,
  getNonNegative,
 )
import Test.QuickCheck.Gen qualified as Gen

-- | @since 2.1.3
instance Arbitrary BuiltinByteString where
  {-# INLINEABLE arbitrary #-}
  arbitrary = toBuiltin @ByteString . fromList <$> arbitrary
  {-# INLINEABLE shrink #-}
  shrink =
    fmap (toBuiltin @ByteString . fromList)
      . shrink
      . toList
      . fromBuiltin

-- | @since 2.1.3
instance CoArbitrary BuiltinByteString where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = coarbitrary . toList . fromBuiltin

-- | @since 2.1.3
instance Function BuiltinByteString where
  {-# INLINEABLE function #-}
  function =
    functionMap
      (toList . fromBuiltin)
      (toBuiltin @ByteString . fromList)

{- | This instance constructs /truly/ random 'Data'. In most cases, you probably
 want something more specific.

 = Note

 The shrinker shrinks \'within\' a 'Data' constructor: thus, 'B' will shrink
 to 'B', 'I' will shrink to 'I', etc. 'Constr' is treated specially: shrinks
 will proceed in two ways:

 - Shrinking to a \'smaller\' constructor tag; and
 - Shrinking the arg list.

 @since 2.1.3
-}
instance Arbitrary Data where
  {-# INLINEABLE arbitrary #-}
  arbitrary = Gen.sized go
    where
      go :: Int -> Gen Data
      go size
        | size <= 0 =
            Gen.oneof
              [ B . fromList <$> vectorOfUpTo 64 arbitrary
              , I <$> arbitrary
              ]
        | otherwise =
            Gen.oneof
              [ B . fromList <$> vectorOfUpTo 64 arbitrary
              , I <$> arbitrary
              , List <$> Gen.listOf (go $ size `quot` 2)
              , Map <$> Gen.listOf ((,) <$> go (size `quot` 2) <*> go (size `quot` 2))
              , (Constr . getNonNegative <$> arbitrary) <*> Gen.listOf (go $ size `quot` 2)
              ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    B bs -> B . fromList <$> (shrink . toList $ bs)
    I i -> I <$> shrink i
    List xs -> List <$> shrink xs
    Map kvs -> Map <$> shrink kvs
    Constr ix args ->
      (Constr . getNonNegative <$> (shrink . NonNegative $ ix)) <*> shrink args

-- | @since 2.1.3
instance CoArbitrary Data where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary dat = case dat of
    I i -> Gen.variant (0 :: Int) . coarbitrary i
    B bs -> Gen.variant (1 :: Int) . coarbitrary (toList bs)
    List xs -> Gen.variant (2 :: Int) . coarbitrary xs
    Map kvs -> Gen.variant (3 :: Int) . coarbitrary kvs
    Constr ix args -> Gen.variant (4 :: Int) . coarbitrary ix . coarbitrary args

-- | @since 2.1.3
instance Function Data where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        Data ->
        Either
          (Either Integer [Word8])
          (Either [Data] (Either [(Data, Data)] (Integer, [Data])))
      into = \case
        I i -> Left . Left $ i
        B bs -> Left . Right . toList $ bs
        List xs -> Right . Left $ xs
        Map kvs -> Right . Right . Left $ kvs
        Constr ix args -> Right . Right . Right $ (ix, args)
      outOf ::
        Either
          (Either Integer [Word8])
          (Either [Data] (Either [(Data, Data)] (Integer, [Data]))) ->
        Data
      outOf = \case
        Left (Left i) -> I i
        Left (Right w8s) -> B . fromList $ w8s
        Right (Left xs) -> List xs
        Right (Right (Left kvs)) -> Map kvs
        Right (Right (Right (ix, args))) -> Constr ix args

-- | @since 2.1.3
deriving via Integer instance Arbitrary DiffMilliSeconds

-- | @since 2.1.3
deriving via Integer instance CoArbitrary DiffMilliSeconds

-- | @since 2.1.3
instance Function DiffMilliSeconds where
  -- We have to do it this way, because via-derivation fails due to the
  -- non-contravariance of :->.
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @Integer) coerce

-- | @since 2.1.3
deriving via BuiltinByteString instance Arbitrary LedgerBytes

-- | @since 2.1.3
deriving via BuiltinByteString instance CoArbitrary LedgerBytes

-- | @since 2.1.3
instance Function LedgerBytes where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

-- | @since 2.1.3
deriving via (NonNegative Integer) instance Arbitrary POSIXTime

-- | @since 2.1.3
deriving via Integer instance CoArbitrary POSIXTime

-- | @since 2.1.3
instance Function POSIXTime where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @Integer) coerce

-- Note from Koz: We write these instances 'by hand' as, while 'BuiltinData'
-- does expose its constructor, via-derivations are blocked. Not sure why this
-- is.

-- | @since 2.1.3
instance Arbitrary BuiltinData where
  {-# INLINEABLE arbitrary #-}
  arbitrary = dataToBuiltinData <$> arbitrary
  {-# INLINEABLE shrink #-}
  shrink = fmap dataToBuiltinData . shrink . builtinDataToData

-- | @since 2.1.3
instance CoArbitrary BuiltinData where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary dat = coarbitrary (builtinDataToData dat)

-- | @since 2.1.3
instance Function BuiltinData where
  {-# INLINEABLE function #-}
  function = functionMap builtinDataToData dataToBuiltinData

{- | This is based on 'DatumHash' being a Blake2b-256 hash, which is 32 bytes
 long. This type does not shrink, as it wouldn't really make much sense to.

 @since 2.1.3
-}
instance Arbitrary DatumHash where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    inner <- fromListN 32 <$> Gen.vectorOf 32 arbitrary
    pure . DatumHash . toBuiltin @ByteString $ inner

-- | @since 2.1.3
deriving via BuiltinByteString instance CoArbitrary DatumHash

-- | @since 2.1.3
instance Function DatumHash where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

-- | @since 2.1.3
deriving via BuiltinData instance Arbitrary Datum

-- | @since 2.1.3
deriving via BuiltinData instance CoArbitrary Datum

-- | @since 2.1.3
instance Function Datum where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinData) coerce

{- | This is based on 'PubKeyHash' being a Blake2b-224 hash, which is 28 bytes
 long. This type does not shrink, as it wouldn't really make much sense to.

 @since 2.1.3
-}
instance Arbitrary PubKeyHash where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    inner <- fromListN 28 <$> Gen.vectorOf 28 arbitrary
    pure . PubKeyHash . toBuiltin @ByteString $ inner

-- | @since 2.1.3
deriving via BuiltinByteString instance CoArbitrary PubKeyHash

-- | @since 2.1.3
instance Function PubKeyHash where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

{- | This will generate either a 'PubKeyCredential' or a 'ScriptCredential' with
 equal probability. As neither 'PubKeyHash' nor 'ValidatorHash' shrink, this
 type doesn't either.

 @since 2.1.3
-}
instance Arbitrary Credential where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Gen.oneof
      [ PubKeyCredential <$> arbitrary
      , ScriptCredential <$> arbitrary
      ]

-- | @since 2.1.3
instance CoArbitrary Credential where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary cred = case cred of
    PubKeyCredential phk -> Gen.variant (0 :: Int) . coarbitrary phk
    ScriptCredential vh -> Gen.variant (1 :: Int) . coarbitrary vh

-- | @since 2.1.3
instance Function Credential where
  {-# INLINEABLE function #-}
  function = functionMap into $ \case
    Left pkh -> PubKeyCredential pkh
    Right vh -> ScriptCredential vh
    where
      into :: Credential -> Either PubKeyHash ScriptHash
      into = \case
        PubKeyCredential pkh -> Left pkh
        ScriptCredential vh -> Right vh

{- | This is based on 'TxId' being a Blake2b-256 hash, which is 32 bytes
 long. This type does not shrink, as it wouldn't really make much sense to.

 @since 2.1.3
-}
deriving via DatumHash instance Arbitrary TxId

-- | @since 2.1.3
deriving via DatumHash instance CoArbitrary TxId

-- | @since 2.1.3
instance Function TxId where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

{- | This will generate a zero-based index for the 'txOutRefIdx` field.
 Furthermore, the shrinker will shrink /only/ in the 'txOutRefIdx' field
 toward zero, as 'TxId's do not shrink.

 @since 2.1.3
-}
instance Arbitrary TxOutRef where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    id' <- arbitrary
    NonNegative idx <- arbitrary
    pure $ TxOutRef {txOutRefId = id', txOutRefIdx = idx}
  {-# INLINEABLE shrink #-}
  shrink tor = do
    NonNegative idx' <- shrink . NonNegative . txOutRefIdx $ tor
    pure $ tor {txOutRefIdx = idx'}

-- | @since 2.1.3
instance CoArbitrary TxOutRef where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary tor = coarbitrary (txOutRefId tor) . coarbitrary (txOutRefIdx tor)

-- | @since 2.1.3
instance Function TxOutRef where
  {-# INLINEABLE function #-}
  function = functionMap into $ \(id', idx) ->
    TxOutRef
      { txOutRefId = id'
      , txOutRefIdx = idx
      }
    where
      into :: TxOutRef -> (TxId, Integer)
      into tor = (txOutRefId tor, txOutRefIdx tor)

-- | @since 2.1.3
instance Arbitrary StakingCredential where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Gen.oneof
      [ StakingHash <$> arbitrary
      , StakingPtr <$> go <*> go <*> go
      ]
    where
      -- Based on documentation bounding it to Word64
      go :: Gen Integer
      go = fromIntegral <$> arbitrary @Word64

-- | @since 2.1.3
instance CoArbitrary StakingCredential where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    StakingHash cred -> Gen.variant (0 :: Int) . coarbitrary cred
    StakingPtr i j k ->
      Gen.variant (1 :: Int)
        . coarbitrary i
        . coarbitrary j
        . coarbitrary k

-- | @since 2.1.3
instance Function StakingCredential where
  {-# INLINEABLE function #-}
  function = functionMap into $ \case
    Left cred -> StakingHash cred
    Right (i, j, k) -> StakingPtr i j k
    where
      into :: StakingCredential -> Either Credential (Integer, Integer, Integer)
      into = \case
        StakingHash cred -> Left cred
        StakingPtr i j k -> Right (i, j, k)

{- | As neither 'Credential' nor 'StakingCredential' shrink, this type doesn't
 either.

 @since 2.1.3
-}
instance Arbitrary Address where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    cred <- arbitrary
    scred <- arbitrary
    pure $
      Address
        { addressCredential = cred
        , addressStakingCredential = scred
        }

-- | @since 2.1.3
instance CoArbitrary Address where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary addr =
    coarbitrary (addressCredential addr)
      . coarbitrary (addressStakingCredential addr)

-- | @since 2.1.3
instance Function Address where
  {-# INLINEABLE function #-}
  function = functionMap into $ \(cred, scred) ->
    Address
      { addressCredential = cred
      , addressStakingCredential = scred
      }
    where
      into :: Address -> (Credential, Maybe StakingCredential)
      into addr = (addressCredential addr, addressStakingCredential addr)

{- | This generates only those 'TokenName's corresponding to ASCII strings. This
 is somewhat limited, but otherwise would require UTF-8 encoding as part of
 the generator. It would also significantly complicate shrinks: we would have
 to re-encode, shrink, then decode again.

 @since 2.1.3
-}
instance Arbitrary TokenName where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    TokenName <$> do
      ASCIIString name <- arbitrary
      pure
        . toBuiltin @ByteString
        . fromList
        . fmap (fromIntegral . ord)
        $ name
  {-# INLINEABLE shrink #-}
  shrink (TokenName name) = do
    let name' =
          ASCIIString
            . fmap (chr . fromIntegral)
            . toList
            . fromBuiltin
            $ name
    ASCIIString name'' <- shrink name'
    pure
      . TokenName
      . toBuiltin @ByteString
      . fromList
      . fmap (fromIntegral . ord)
      $ name''

-- | @since 2.1.3
deriving via BuiltinByteString instance CoArbitrary TokenName

-- | @since 2.3.1
instance Function TokenName where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

{- | The shrinker for this type will not shrink \'out-of-arm\'. Effectively,
 this means 'NoOutputDatum' does not shrink, 'OutputDatumHash' also does not
 shrink (as 'DatumHash' doesn't), and 'OutputDatum' will shrink to other
 'OutputDatum's.

 @since 2.1.3
-}
instance Arbitrary OutputDatum where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    Gen.oneof
      [ pure NoOutputDatum
      , OutputDatumHash <$> arbitrary
      , OutputDatum <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    NoOutputDatum -> []
    OutputDatumHash _ -> []
    OutputDatum dat -> OutputDatum <$> shrink dat

-- | @since 2.1.3
instance CoArbitrary OutputDatum where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary od = case od of
    NoOutputDatum -> Gen.variant (0 :: Int)
    OutputDatumHash dh -> Gen.variant (1 :: Int) . coarbitrary dh
    OutputDatum dat -> Gen.variant (2 :: Int) . coarbitrary dat

-- | @since 2.1.3
instance Function OutputDatum where
  {-# INLINEABLE function #-}
  function = functionMap into $ \case
    Nothing -> NoOutputDatum
    Just (Left dh) -> OutputDatumHash dh
    Just (Right dat) -> OutputDatum dat
    where
      into :: OutputDatum -> Maybe (Either DatumHash Datum)
      into = \case
        NoOutputDatum -> Nothing
        OutputDatumHash dh -> Just . Left $ dh
        OutputDatum dat -> Just . Right $ dat

{- | This is based on 'ScriptHash' being a Blake2b-224 hash, which is 28 bytes
 long. This type does not shrink, as it wouldn't really make much sense to.

 @since 2.1.3
-}
deriving via PubKeyHash instance Arbitrary ScriptHash

-- | @since 2.1.3
deriving via PubKeyHash instance CoArbitrary ScriptHash

-- | @since 2.1.3
instance Function ScriptHash where
  {-# INLINEABLE function #-}
  function = functionMap (coerce @_ @BuiltinByteString) coerce

-- | @since 2.1.3
instance Arbitrary TxOut where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    addr <- arbitrary
    GenValue val <- arbitrary @(GenValue NonNegative Positive)
    outDatum <- arbitrary
    refScript <- arbitrary
    pure $
      TxOut
        { txOutAddress = addr
        , txOutValue = val
        , txOutDatum = outDatum
        , txOutReferenceScript = refScript
        }
  {-# INLINEABLE shrink #-}
  shrink txOut = do
    -- We skip Address, as it doesn't shrink anyway
    GenValue val' :: GenValue NonNegative Positive <-
      shrink . GenValue . txOutValue $ txOut
    outDatum' <- shrink . txOutDatum $ txOut
    refScript' <- shrink . txOutReferenceScript $ txOut
    pure $
      txOut
        { txOutValue = val'
        , txOutDatum = outDatum'
        , txOutReferenceScript = refScript'
        }

-- | @since 2.1.3
instance CoArbitrary TxOut where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary txOut =
    coarbitrary (txOutAddress txOut)
      . coarbitrary ((GenValue . txOutValue $ txOut) :: GenValue NonNegative Positive)
      . coarbitrary (txOutDatum txOut)
      . coarbitrary (txOutReferenceScript txOut)

-- | @since 2.1.3
instance Function TxOut where
  {-# INLINEABLE function #-}
  function = functionMap into $ \(addr, GenValue val, outDatum, refScript) ->
    TxOut
      { txOutAddress = addr
      , txOutValue = val
      , txOutDatum = outDatum
      , txOutReferenceScript = refScript
      }
    where
      into ::
        TxOut ->
        (Address, GenValue NonNegative Positive, OutputDatum, Maybe ScriptHash)
      into txOut =
        ( txOutAddress txOut
        , GenValue . txOutValue $ txOut
        , txOutDatum txOut
        , txOutReferenceScript txOut
        )

-- Helpers

-- Similar to 'vectorOf', but instead generates a size-dependent length up to
-- the specified maximum (or the size, whichever is smaller).
vectorOfUpTo ::
  forall (a :: Type).
  Int ->
  Gen a ->
  Gen [a]
vectorOfUpTo lim gen = Gen.sized $ \size -> do
  len <- Gen.chooseInt (0, min size lim)
  Gen.vectorOf len gen
