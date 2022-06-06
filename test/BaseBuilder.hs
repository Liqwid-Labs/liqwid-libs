{-# LANGUAGE ViewPatterns #-}

module BaseBuilder (
  BuilderInterface(..),
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

import Control.Applicative
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Hash (sha2_256)
import Plutarch.Api.V1
import Plutarch.Context
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value
import Test.Tasty.QuickCheck

data BuilderInterface
    = InputFromPubKey PubKeyHash Value
    | InputFromPubKeyWith PubKeyHash Value Integer
    | InputFromScript ValidatorHash Value
    | InputFromScriptWith ValidatorHash Value Integer
    | OutputToPubKey PubKeyHash Value
    | OutputToPubKeyWith PubKeyHash Value Integer
    | OutputToScript ValidatorHash Value
    | OutputToScriptWith ValidatorHash Value Integer
    | SignedWith PubKeyHash
    | Mint Value
    | ExtraData Integer
    deriving stock (Show, Eq)

toBuilder :: Builder a => BuilderInterface -> a
toBuilder (InputFromPubKey pkh val)              = inputFromPubKey pkh val
toBuilder (InputFromPubKeyWith pkh val dat)      = inputFromPubKeyWith pkh val dat
toBuilder (InputFromScript valhash val)          = inputFromScript valhash val
toBuilder (InputFromScriptWith valhash val dat)  = inputFromScriptWith valhash val dat
toBuilder (OutputToPubKey pkh val)               = outputToPubKey pkh val
toBuilder (OutputToPubKeyWith pkh val dat)       = outputToPubKeyWith pkh val dat
toBuilder (OutputToScript valhash val)           = outputToScript valhash val
toBuilder (OutputToScriptWith valhash val dat)   = outputToScriptWith valhash val dat
toBuilder (SignedWith pkh)                       = signedWith pkh
toBuilder (Mint val)                             = mint val
toBuilder (ExtraData dat)                        = extraData dat

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

instance Arbitrary BuilderInterface where
    arbitrary = do
        pkh <- PubKeyHash . toBuiltin <$> genHashByteString
        valhash <- ValidatorHash . toBuiltin <$> genHashByteString
        val <- genAnyValue
        dat <- arbitrary

        elements
            [ InputFromPubKey pkh val
            , InputFromPubKeyWith pkh val dat
            , InputFromScript valhash val    
            , InputFromScriptWith valhash val dat
            , OutputToPubKey pkh val        
            , OutputToPubKeyWith pkh val dat
            , OutputToScript valhash val     
            , OutputToScriptWith valhash val dat
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

checkWithDatum :: (ToData a) => (Address, Value, a) -> [TxOut] -> Bool
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
    vhashToAddr = (flip Address Nothing) . ScriptCredential

    mintingRule = minted infs == (txInfoMint . scriptContextTxInfo $ context)

    go (InputFromPubKey (pkToAddr -> addr) val) =
        property $
            check (addr, val) ins
    go (InputFromPubKeyWith (pkToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) ins
                && datumExists datumPairs dat
    go (InputFromScript (vhashToAddr -> addr) val) =
        property $
            check (addr, val) ins
    go (InputFromScriptWith (vhashToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) ins
                && datumExists datumPairs dat                
    go (OutputToPubKey (pkToAddr -> addr) val) =
        property $
            check (addr, val) outs
    go (OutputToPubKeyWith (pkToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) outs
                && datumExists datumPairs dat
    go (OutputToScript (vhashToAddr -> addr) val) =
        property $
            check (addr, val) outs
    go (OutputToScriptWith (vhashToAddr -> addr) val dat) =
        property $
            checkWithDatum (addr, val, dat) outs
                && datumExists datumPairs dat                
    go (SignedWith pk) =
        property $
            let signers = txInfoSignatories . scriptContextTxInfo $ context
             in elem pk signers
    go (Mint _val) = property $ True
    go (ExtraData dat) =
        property $
            datumExists datumPairs dat
