{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Oracle where

import           Data.String               (IsString (fromString), String)
import           Plutus.V1.Ledger.Value    (AssetClass (AssetClass),
                                            assetClassValueOf)
import           Plutus.V2.Ledger.Api      (BuiltinData, Datum (Datum),
                                            OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
                                            PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInInfo (txInInfoResolved), TxInfo,
                                            TxOut (txOutDatum, txOutValue),
                                            UnsafeFromData (unsafeFromBuiltinData),
                                            Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (findDatum, findOwnInput,
                                            getContinuingOutputs, txSignedBy)
import           PlutusTx                  (CompiledCode,
                                            FromData (fromBuiltinData),
                                            applyCode, compile, liftCode,
                                            makeLift, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, Eq (..), Integer, Maybe (..),
                                            isJust, tail, take, traceError,
                                            traceIfFalse, ($), (&&), (.))
import           Prelude                   (IO, Show (show), span)
import qualified Prelude                   ((/=))
import           Text.Printf               (printf)
import           Utilities                 (wrapValidator, writeCodeToFile,
                                            writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

--  We have extracted this function, so it can be used in other validators.
-- It tries to parse the datum, it returns Nothing if it fails.
{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer
parseOracleDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info
                        PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This is what we parameterize our oracle with.
-- The NFT, and the operator that is able to update and delete the oracle.
data OracleParams = OracleParams
    { oNFT      :: AssetClass
    , oOperator :: PubKeyHash
    }
PlutusTx.makeLift ''OracleParams

data OracleRedeemer = Update | Delete
    deriving Prelude.Show
PlutusTx.unstableMakeIsData ''OracleRedeemer

-- The rate of the USD/Ada, (how many cents is 1 Ada worth)
-- Oracle Datum
type Rate = Integer

-- Here we ignore the rate, we read it as reference input when used in other validators,
-- but this validator doesn't really know to the exact value of the rate. just that's its a valid number.
{-# INLINABLE mkValidator #-}
mkValidator :: OracleParams -> Rate -> OracleRedeemer -> ScriptContext -> Bool
mkValidator oracle _ r ctx =
    case r of
        Update -> traceIfFalse "token missing from input"   inputHasToken  && -- We want the token both as input
                  traceIfFalse "token missing from output"  outputHasToken && -- and as output. (coming in and coming out)
                  traceIfFalse "operator signature missing" checkOperatorSignature &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Delete -> traceIfFalse "operator signature missing" checkOperatorSignature

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- | Check that the 'oracle' is signed by the 'oOperator'.
    checkOperatorSignature :: Bool
    checkOperatorSignature = txSignedBy info $ oOperator oracle

    -- | Find the oracle input.
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    -- Check that the oracle input contains the NFT.
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT oracle) == 1

    -- | Find the oracle output.
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    -- Check that the oracle output contains the NFT.
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oNFT oracle) == 1

    -- Check that the oracle output contains a valid datum.
    validOutputDatum :: Bool
    validOutputDatum = isJust $ parseOracleDatum ownOutput info


---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator


validator :: OracleParams -> Validator
validator oracle = mkValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oracle


{-# INLINABLE  mkWrappedValidatorLucid #-}
--                            CS              TN           operator        rate          redeemer       context
mkWrappedValidatorLucid :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs tn pkh = wrapValidator $ mkValidator op
    where
        op = OracleParams
            { oNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)
            , oOperator   = unsafeFromBuiltinData pkh
            }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( compile [|| mkWrappedValidatorLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

saveOracleCode :: IO ()
saveOracleCode = writeCodeToFile "assets/oracle.plutus" validatorCode

saveOracleScript :: String -> PubKeyHash -> IO ()
saveOracleScript symbol pkh = do
    let
    writeValidatorToFile fp $ validator op
    where
        op = OracleParams
            { oNFT= parseToken symbol
            , oOperator   = pkh
            }
        fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

parseToken :: String -> AssetClass
parseToken s =
  let
    (x, y) = span (Prelude./= '.') s
  in
    AssetClass (fromString x, fromString $ tail y)
