{-# LANGUAGE ViewPatterns #-}

module BaseBuilder (
    BuilderInterface (..),
    toBuilder,
    build,
    baseRules,
    genAnyValue,
    genValueWithCS,
    genCurrencySymbol,
    genPrettyByteString,
    genHashByteString,
    toDatumHash,
    searchDatum,
    check,
    checkWithDatum,
    datumExists,
) where

import Control.Applicative (Applicative (liftA2))
import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Data.ByteString.Hash (sha2_256)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Context
import PlutusLedgerApi.V1 (
    Address (Address),
    Credential (..),
    CurrencySymbol,
    Data,
    Datum (Datum),
    DatumHash,
    FromData,
    PubKeyHash (PubKeyHash),
    ScriptContext (scriptContextTxInfo),
    ToData (..),
    TxId (..),
    TxInInfo (txInInfoResolved),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
    TxOut (txOutAddress, txOutDatumHash, txOutValue),
    ValidatorHash (..),
    Value,
    fromData,
    singleton,
    toBuiltin,
    toData,
 )
import PlutusLedgerApi.V1.Value (
    AssetClass (AssetClass),
    assetClassValue,
    currencySymbol,
    tokenName,
 )
import Test.Tasty.QuickCheck

data UTXOComponents
    = OfCredential Credential
    | OfPubKey PubKeyHash
    | OfScript ValidatorHash
    | AmountOf Value
    | OutputOf TxId
    | RefIndexOf Integer
    | With Integer
    deriving stock (Show, Eq)

data BuilderInterface
    = To [UTXOComponents]
    | From [UTXOComponents]
    | SignedWith PubKeyHash
    | Mint Value
    | ExtraData Integer
    deriving stock (Show, Eq)

toUTXO :: [UTXOComponents] -> (UTXO -> UTXO)
toUTXO xs = foldr (.) id $ go <$> xs
  where
    go :: UTXOComponents -> (UTXO -> UTXO)
    go (OfCredential cred) = credential cred
    go (OfPubKey pkh) = pubKey pkh
    go (OfScript vh) = script vh
    go (AmountOf val) = amountOf val
    go (OutputOf tid) = outputOf tid
    go (RefIndexOf tidx) = refIndexOf tidx
    go (With dat) = with dat

toBuilder :: Builder a => BuilderInterface -> a
toBuilder (To (toUTXO -> comps)) = to comps
toBuilder (From (toUTXO -> comps)) = from comps
toBuilder (SignedWith pkh) = signedWith pkh
toBuilder (Mint val) = mint val
toBuilder (ExtraData dat) = extraData dat

genHashByteString :: Gen C.ByteString
genHashByteString = sha2_256 . C.pack . show <$> (chooseAny :: Gen Integer)

genValue :: AssetClass -> Gen Value
genValue ac = assetClassValue ac . abs <$> (arbitrary :: Gen Integer)

genValueWithCS :: CurrencySymbol -> Gen Value
genValueWithCS cs = do
    tk <- tokenName <$> genPrettyByteString
    val <- abs <$> (arbitrary :: Gen Integer)
    return $ singleton cs tk val

genPrettyByteString :: Gen C.ByteString
genPrettyByteString = C.pack <$> (listOf1 $ elements ['a' .. 'z'])

genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = currencySymbol <$> genHashByteString

genAssetClass :: Gen AssetClass
genAssetClass =
    AssetClass
        <$> liftA2
            (,)
            (currencySymbol <$> genHashByteString)
            (tokenName <$> genPrettyByteString)

genAnyValue :: Gen Value
genAnyValue = genAssetClass >>= genValue

genCredential :: Gen Credential
genCredential = do
    pkh <- PubKeyHash . toBuiltin <$> genHashByteString
    vh <- ValidatorHash . toBuiltin <$> genHashByteString

    elements [PubKeyCredential pkh, ScriptCredential vh]

instance Arbitrary BuilderInterface where
    arbitrary = do
        pkh <- PubKeyHash . toBuiltin <$> genHashByteString
        vh <- ValidatorHash . toBuiltin <$> genHashByteString
        txId <- TxId . toBuiltin <$> genHashByteString
        txIdx <- arbitrary
        cred <- genCredential
        val <- genAnyValue
        dat <- arbitrary

        possession <-
            elements
                [ OfCredential cred
                , OfPubKey pkh
                , OfScript vh
                ]

        other <-
            sublistOf
                [ OutputOf txId
                , AmountOf val
                , RefIndexOf txIdx
                , With dat
                ]

        elements
            [ From (possession : other)
            , To (possession : other)
            , SignedWith pkh
            , Mint val
            , ExtraData dat
            ]

build :: Builder a => [BuilderInterface] -> a
build xs = mconcat $ toBuilder <$> xs

-- Arbitrary data to DatumHash helper
toDatumHash :: (ToData a) => a -> DatumHash
toDatumHash = datumHash . Datum . toBuiltinData

-- Search given DatumHash in TxInfoData. Returns the matching data
-- if exists.
searchDatum :: [(DatumHash, Datum)] -> DatumHash -> Maybe Data
searchDatum datumPairs dh
    | null filtered = Nothing
    | otherwise = Just . toData . snd . head $ filtered
  where
    filtered = filter ((dh ==) . fst) datumPairs

-- search given address, value pair in given list of TxOut
check :: (Address, Value) -> [TxOut] -> Bool
check (addr, val) os
    | null filtered = False
    | otherwise = True
  where
    filtered =
        filter
            ( \x ->
                addr == txOutAddress x
                    && val == txOutValue x
            )
            os

checkWithDatum :: ToData a => (Address, Value, a) -> [TxOut] -> Bool
checkWithDatum (addr, val, dat) os
    | null filtered = False
    | otherwise = True
  where
    filtered =
        filter
            ( \x ->
                addr == txOutAddress x
                    && val == txOutValue x
                    && maybe False (toDatumHash dat ==) (txOutDatumHash x)
            )
            os

-- Check if given data is currectly presented in ScriptContext.
datumExists :: (FromData a, Eq a, ToData a) => [(DatumHash, Datum)] -> a -> Bool
datumExists datumPairs val =
    case searchDatum datumPairs dh of
        Just (fromData -> Just x) -> x == val
        _ -> False
  where
    dh = toDatumHash val

minted :: [BuilderInterface] -> Value
minted xs = mconcat $ go <$> xs
  where
    go (Mint val) = val
    go _ = mempty

baseRules :: ScriptContext -> [BuilderInterface] -> Property
baseRules context infs = (conjoin $ go <$> infs) .&&. mintingRule
  where
    ins = txInInfoResolved <$> (txInfoInputs . scriptContextTxInfo $ context)
    outs = txInfoOutputs . scriptContextTxInfo $ context
    datumPairs = txInfoData . scriptContextTxInfo $ context

    pkToAddr = (flip Address Nothing) . PubKeyCredential
    vhToAddr = (flip Address Nothing) . ScriptCredential

    mintingRule = minted infs == (txInfoMint . scriptContextTxInfo $ context)

    utxoRules utxo =
        check (Address (utxoCredential utxo) Nothing, utxoValue utxo)

    go (To xs) =
        property $
            check (Address (utxoCredential utxo) Nothing, utxoValue utxo) outs -- .&&.
            --      property (maybe True checkDat (utxoData utxo))
      where
        utxo = toUTXO xs $ (UTXO (PubKeyCredential "") mempty Nothing Nothing Nothing)
    --        checkDat d = checkWithDatum (Address (utxoCredential utxo) Nothing, utxoValue utxo, d) outs
    go (From xs) = property True
    go (SignedWith pk) =
        property $
            let signers = txInfoSignatories . scriptContextTxInfo $ context
             in elem pk signers
    go (Mint _val) = property $ True
    go (ExtraData dat) =
        property $
            datumExists datumPairs dat
