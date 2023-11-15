{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module Bid where

import           Plutus.V1.Ledger.Value    (AssetClass (AssetClass),
                                            assetClassValueOf)
import           Plutus.V2.Ledger.Api      (BuiltinData, Datum (Datum),
                                            OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
                                            PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo,
                                            UnsafeFromData (unsafeFromBuiltinData),
                                            Validator, Value, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (findDatum, txSignedBy, valuePaidTo)
import           PlutusTx                  (CompiledCode,
                                            FromData (fromBuiltinData),
                                            applyCode, compile, liftCode,
                                            makeLift, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, Eq ((==)), Maybe (..),
                                            traceError, traceIfFalse, ($), (.))
import qualified Prelude
import           Utilities                 (wrapValidator, writeCodeToFile,
                                            writeValidatorToFile)



---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Bid's Parameters
data BidParams = BidParams
    {
     dataTokenNFT :: AssetClass
    }
PlutusTx.makeLift ''BidParams


-- Datum containing all the relevant information
data BidDatum = BidDatum
    {
     dataBuyer   :: PubKeyHash
    } deriving Prelude.Show
unstableMakeIsData ''BidDatum

-- We can redeem our own bidding (if we are the buyer) or sell the token to the buyer(if we are the seller)
data BidRedeemer = Redeem | Sell
unstableMakeIsData ''BidRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: BidParams -> BidDatum -> BidRedeemer -> ScriptContext -> Bool
mkValidator p dat r ctx = case r of
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
    buyerGetsToken = assetClassValueOf valuePaidToBuyer (dataTokenNFT p) == 1

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BidParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: BidParams -> Validator
validator params= mkValidatorScript $
    $$(compile [|| mkWrappedValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params

{-# INLINABLE mkWrappedValidatorLucid #-}
--                         CurrencySymbol  TokenName,    datum             redeemer      context
mkWrappedValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs tn = wrapValidator $ mkValidator bidParams
    where
        bidParams = BidParams
            { dataTokenNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)
            }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkWrappedValidatorLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------ SERIALIZE VALIDATOR ------------------------------------

saveBidCode :: Prelude.IO ()
saveBidCode = writeCodeToFile "assets/bid.plutus" validatorCode
