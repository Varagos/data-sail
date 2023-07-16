{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module DataListing where

import           Plutus.V1.Ledger.Value    (adaSymbol, adaToken, valueOf)
import           Plutus.V2.Ledger.Api      (Address, BuiltinData, Datum (Datum),
                                            OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoOutputs),
                                            TxOut (txOutAddress, txOutValue),
                                            Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (findDatum)
import           PlutusTx                  (FromData (fromBuiltinData), compile,
                                            unstableMakeIsData)
import           PlutusTx.Prelude          (Bool (..), Eq (..), Integer,
                                            Maybe (..), Ord ((>=)), traceError,
                                            traceIfFalse)
import qualified Prelude
import           Utilities                 (wrapValidator, writeValidatorToFile)


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseDataListingDatum #-}
parseDataListingDatum :: OutputDatum -> TxInfo -> Maybe DataListDatum
parseDataListingDatum o info = case o of
    NoOutputDatum         -> traceError "Found DataListing output but NoOutputDatum"
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh    -> do
                           Datum d <- findDatum dh info
                           PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Datum containing all the relevant information
data DataListDatum = DataListDatum
    { dataOwner :: Address
    , price     :: Integer
    } deriving Prelude.Show
unstableMakeIsData ''DataListDatum

-- We can lock or redeem our own collateral or liquidate someone else's
data DataListingRedeemer = Redeem | Purchase
unstableMakeIsData ''DataListingRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: DataListDatum -> DataListingRedeemer -> ScriptContext -> Bool
mkValidator dat r ctx = case r of
    Purchase -> traceIfFalse "amount required not paid to owner" checkPricePaidToOwner
    Redeem    -> traceError "Redeem not implemented"
        -- traceIfFalse "data owner's signature missing" checkSignedByCollOwner &&
        --          traceIfFalse "burned stablecoin amount mismatch" checkStablecoinAmount

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    txOutToOwner :: TxOut
    txOutToOwner = case Prelude.filter (\o -> txOutAddress o == dataOwner dat) txOutputs of
        []  -> traceError "No output to collateral owner"
        [o] -> o
        _   -> traceError "More than one output to collateral owner"

    -- In lovelaces
    dataPrice :: Integer
    dataPrice = price dat

    -- This is in Lovelaces
    pricePaidToOwner :: Integer
    pricePaidToOwner = valueOf (txOutValue txOutToOwner) adaSymbol adaToken

    -- Check the price asked in ADA was paid to the data owner
    checkPricePaidToOwner :: Bool
    checkPricePaidToOwner = pricePaidToOwner >= dataPrice


---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

saveDataListingScript :: Prelude.IO ()
saveDataListingScript = writeValidatorToFile "assets/data-listing.plutus" validator
