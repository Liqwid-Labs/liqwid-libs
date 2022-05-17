module Plutarch.Context.Internal (
    InputPosition (..),
    TransactionConfig (..),
    UTXOType (..),
    SideUTXO (..),
    ValidatorUTXO (..),
    ValueType (..),
    defaultConfig,
) where

import Data.Kind (Type)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval (Interval, always)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Tx (TxId (TxId))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value)
import PlutusCore.Data (Data)

{- | Parameters necessary to build a context.

 @since 1.0.0
-}
data TransactionConfig = TransactionConfig
    { testFee :: Value
    , testTimeRange :: Interval POSIXTime
    , testTxId :: TxId
    , testCurrencySymbol :: CurrencySymbol
    , testValidatorHash :: ValidatorHash
    , testInputPosition :: InputPosition
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | Where to put the validated input in the 'txInfoInputs' when generating a
 'ScriptContext'.

 @since 1.0.0
-}
data InputPosition = AtFront | AtBack
    deriving stock
        ( -- | @since 1.0.0
          Eq
        , -- | @since 1.0.0
          Show
        )

{- | A \'baseline\' 'TransactionConfig'. You can modify this to suit your needs
 if you want something more specific.

 @since 1.0.0
-}
defaultConfig :: TransactionConfig
defaultConfig =
    TransactionConfig mempty always (TxId "abcd") "ff" "90ab" AtFront

{- | Metadata of UTxOs at different types of address.

 @since 1.0.0
-}
data UTXOType
    = -- | Metadata at a 'PubKeyHash' address.
      --
      -- @since 1.0.0
      PubKeyUTXO PubKeyHash (Maybe Data)
    | -- | Metadata at a 'ValidatorHash' address.
      --
      -- @since 1.0.0
      ScriptUTXO ValidatorHash Data
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | Different varieties of value.

 @since 1.0.0
-}
data ValueType
    = GeneralValue Value
    | TokensValue TokenName Integer
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | A UTxO involved in an input or output that /isn't/ the \'primary\' one for
 the script that a context will be used with.

 = Note

 Do /not/ use this to represent the UTxO at the address of the validator (or
 minting policy) that the script context will be used with. If this is what
 you want, use 'ValidatorUTXO' or 'SpendUTXO' instead.

 As an additional form of safety, any 'SideUTXO' whose address is the same as
 the 'testValidatorHash' of the 'TransactionConfig' being used to build a
 script context will be discarded.

 @since 1.0.0
-}
data SideUTXO = SideUTXO UTXOType ValueType
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | A UTxO at the address of the validator for which the script context will be
 used.

 @since 1.0.0
-}
data ValidatorUTXO (datum :: Type) = ValidatorUTXO datum Value
    deriving stock
        ( -- | @since 1.0.0
          Show
        )
