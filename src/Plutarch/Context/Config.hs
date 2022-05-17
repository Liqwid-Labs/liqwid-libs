module Plutarch.Context.Config (
    -- * Type
    ContextConfig (..),

    -- * Default
    defaultConfig,
) where

import Plutus.V1.Ledger.Interval (Interval, always)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Tx (TxId (TxId))
import Plutus.V1.Ledger.Value (CurrencySymbol, Value)

{- | Parameters necessary to build a context.

 @since 1.0.0
-}
data ContextConfig = ContextConfig
    { configFee :: Value
    -- ^ The transaction fee to charge.
    --
    -- @since 1.0.0
    , configTimeRange :: Interval POSIXTime
    -- ^ Valid time range for transaction.
    --
    -- @since 1.0.0
    , configTxId :: TxId
    -- ^ 'TxId' for the transaction.
    --
    -- @since 1.0.0
    , configCurrencySymbol :: CurrencySymbol
    -- ^ 'CurrencySymbol' for the transaction.
    --
    -- @since 1.0.0
    , configValidatorHash :: ValidatorHash
    -- ^ Hash of the script this will be used with.
    --
    -- @since 1.0.0
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | A \'baseline\' 'ContextConfig'. You can modify this to suit your needs
 if you want something more specific.

 @since 1.0.0
-}
defaultConfig :: ContextConfig
defaultConfig = ContextConfig mempty always (TxId "abcd") "ff" "90ab"
