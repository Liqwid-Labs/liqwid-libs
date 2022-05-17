module Plutarch.Context.Internal (
    TransactionConfig (..),
    InputPosition (..),
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
