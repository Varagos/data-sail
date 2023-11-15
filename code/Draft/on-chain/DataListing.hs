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
import           Plutus.V2.Ledger.Api      (Address (addressCredential),
                                            BuiltinData,
                                            Credential (PubKeyCredential, ScriptCredential),
                                            Datum (Datum),
                                            OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
                                            PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInInfo (txInInfoResolved),
                                            TxInfo (txInfoInputs),
                                            TxOut (txOutAddress), Validator,
                                            ValidatorHash, Value,
                                            mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (findDatum, ownHash, valuePaidTo, txSignedBy)
import           PlutusTx                  (FromData (fromBuiltinData), compile,
                                            unstableMakeIsData)
import           PlutusTx.Prelude          (Bool (False), Eq ((==)), Integer,
                                            Maybe (..), Ord ((>=)), filter,
                                            length, map, traceError,
                                            traceIfFalse, ($), (&&), (.))
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
    {
     dataSeller :: PubKeyHash
    , price     :: Integer
    } deriving Prelude.Show
unstableMakeIsData ''DataListDatum

-- We can redeem our own token or purchase someone else's
data DataListingRedeemer = Redeem | Purchase
unstableMakeIsData ''DataListingRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: DataListDatum -> DataListingRedeemer -> ScriptContext -> Bool
mkValidator dat r ctx = case r of
    Purchase -> traceIfFalse "Amount required not paid to owner" buyerHasPaidSeller &&
        traceIfFalse "You must consume only one utxo" consumesOnlyOneUtxo
    Redeem    -> traceIfFalse "data seller's signature missing" checkSignedBySeller

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- In lovelaces
    dataPrice :: Integer
    dataPrice = price dat

    valuePaidToSeller :: Value
    valuePaidToSeller = valuePaidTo info (dataSeller dat)

    pricePaidToSeller :: Integer
    pricePaidToSeller = valueOf valuePaidToSeller adaSymbol adaToken

    buyerHasPaidSeller :: Bool
    buyerHasPaidSeller = pricePaidToSeller >= dataPrice

    consumesOnlyOneUtxo = length consumedInputsOfThisScript == 1
        where
        scriptAddress :: ValidatorHash
        scriptAddress = ownHash ctx

        -- The credentials that are required to unlock each input, can be either PubKeyHash,
        -- which means they belong to a user, and he unlocks them by signing with his pk,
        -- or ScriptCredentials, that require the script to be included, and validated
        inputScriptCredentials :: [Credential]
        inputScriptCredentials = map (addressCredential . txOutAddress . txInInfoResolved) $ txInfoInputs info

        consumedInputsOfThisScript = filter protectedByThisScript inputScriptCredentials
            where
            protectedByThisScript :: Credential -> Bool
            protectedByThisScript c = case c of
                PubKeyCredential _  -> False
                ScriptCredential vh -> vh == scriptAddress

    checkSignedBySeller :: Bool
    checkSignedBySeller = txSignedBy info $ dataSeller dat





---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

saveDataListingScript :: Prelude.IO ()
saveDataListingScript = writeValidatorToFile "assets/data-listing.plutus" validator
