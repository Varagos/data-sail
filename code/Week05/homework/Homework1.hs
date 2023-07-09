{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext (scriptContextTxInfo),
                                       mkMintingPolicyScript, TxInfo (txInfoValidRange), POSIXTimeRange, to)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool , ($), traceIfFalse, (&&))
import           Utilities            (wrapPolicy)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains)

{-# INLINABLE mkDeadlinePolicy #-}
-- Add a second patameter to the Signed policy, of posix time and the condition that minting also happens before the deadline
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy pkh deadline () ctx = traceIfFalse "not signed by who it should be" signedByPublicKeyHash &&
    traceIfFalse "deadline has passed" deadlineHasNotPassed
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx
        signedByPublicKeyHash = txSignedBy txInfo pkh 

        txRange :: POSIXTimeRange
        txRange = txInfoValidRange txInfo

        deadlineHasNotPassed :: Bool
        deadlineHasNotPassed = contains (to deadline) txRange

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
