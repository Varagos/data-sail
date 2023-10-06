{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module Bid where

import           Plutus.V1.Ledger.Value    (AssetClass, assetClassValueOf)
import           Plutus.V2.Ledger.Api      (BuiltinData, Datum (Datum),
                                            OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
                                            PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo, Validator, Value,
                                            mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (findDatum, txSignedBy, valuePaidTo)
import           PlutusTx                  (FromData (fromBuiltinData), compile,
                                            unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, Eq ((==)), Maybe (..),
                                            traceError, traceIfFalse, ($))
import qualified Prelude
import           Utilities                 (wrapValidator, writeValidatorToFile)


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseBidDatum #-}
parseBidDatum :: OutputDatum -> TxInfo -> Maybe BidDatum
parseBidDatum o info = case o of
    NoOutputDatum         -> traceError "Found Bid output but NoOutputDatum"
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh    -> do
                           Datum d <- findDatum dh info
                           PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Datum containing all the relevant information
data BidDatum = BidDatum
    {
     dataTokenNFT :: AssetClass
    , dataBuyer   :: PubKeyHash
    } deriving Prelude.Show
unstableMakeIsData ''BidDatum

-- We can redeem our own bidding (if we are the buyer) or sell the token to the buyer(if we are the seller)
data BidRedeemer = Redeem | Sell
unstableMakeIsData ''BidRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: BidDatum -> BidRedeemer -> ScriptContext -> Bool
mkValidator dat r ctx = case r of
        Sell   -> traceIfFalse "token not given to buyer" buyerGetsToken
        Redeem -> traceIfFalse "buyer's signature missing" checkSignedByBuyer

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSignedByBuyer :: Bool
    checkSignedByBuyer = txSignedBy info $ dataBuyer dat

    valuePaidToBuyer :: Value
    valuePaidToBuyer = valuePaidTo info $ dataBuyer dat

    buyerGetsToken :: Bool
    buyerGetsToken = assetClassValueOf valuePaidToBuyer (dataTokenNFT dat) == 1

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

saveBidScript :: Prelude.IO ()
saveBidScript = writeValidatorToFile "assets/bid.plutus" validator
