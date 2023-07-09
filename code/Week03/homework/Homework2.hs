{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo (txInfoValidRange), from)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool, (.), ($), (&&), traceIfFalse)
import           Utilities            (wrapValidator)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------
-- The datum is POSIXTime, the deadline, this will create one script address per beneficiary
{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = 
    traceIfFalse "not signed by beneficiary" isSignedByBeneficiary && 
    traceIfFalse "deadline has not passed yet" deadlineHasPassed
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        isSignedByBeneficiary :: Bool
        isSignedByBeneficiary = txSignedBy txInfo beneficiary

        deadlineHasPassed :: Bool
        deadlineHasPassed = contains (from deadline) $ txInfoValidRange txInfo

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
