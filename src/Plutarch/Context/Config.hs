{- | Module: Plutarch.Context.Config
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Configuration for context generation.
-}
module Plutarch.Context.Config (
    -- * Type
    ContextConfig (..),

    -- * Default
    defaultConfig,
) where

import PlutusLedgerApi.V1.Interval (Interval, always)
import PlutusLedgerApi.V1.Time (POSIXTime)
import PlutusLedgerApi.V1.Tx (TxId (TxId))
import PlutusLedgerApi.V1.Value (Value)

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
defaultConfig = ContextConfig mempty always (TxId "abcd") -- "ff" "90ab"
