{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash ,
                                       ScriptContext(scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo (txInfoValidRange), from, to)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), otherwise, (.), ($), (&&),  AdditiveSemigroup ((+)))
import           Utilities            (wrapValidator)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx 
    | isSignedByFirstBeneficiary && currentSlotBeforeDeadline = True
    | isSignedBySecondBeneficiary && deadlineHasPassed = True
    | otherwise = False
    where
        isSignedByFirstBeneficiary =  isSignedByBeneficiary . beneficiary1  $ dat
        isSignedBySecondBeneficiary = isSignedByBeneficiary . beneficiary2 $ dat
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        isSignedByBeneficiary :: PubKeyHash -> Bool
        isSignedByBeneficiary = txSignedBy txInfo

        currentSlotBeforeDeadline = contains (to $ deadline dat) $ txInfoValidRange txInfo

        -- deadlineHasPassed = not $ overlaps (to $ deadline dat) $ txInfoValidRange txInfo
        deadlineHasPassed = contains (from (1 + deadline dat)) $ txInfoValidRange txInfo




{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
