{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DataTokenMinting where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (Value, flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&))
import           Prelude                    (IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writeCodeToFile,
                                             writePolicyToFile)

-- A token is parameterized by a utxo, so it can only be minted once

{-# INLINABLE mkDataTokenPolicy #-}
mkDataTokenPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkDataTokenPolicy utxo tn () ctx = traceIfFalse "UTxO not consumed" consumesUtxo &&
                         traceIfFalse "Wrong amount minted" mintsExactlyOneToken
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        transactionInputs :: [TxInInfo]
        transactionInputs = txInfoInputs info

        consumesUtxo :: Bool
        consumesUtxo = any (\i -> txInInfoOutRef i == utxo) transactionInputs

        valueMinted :: Value
        valueMinted = txInfoMint info

        mintsExactlyOneToken :: Bool
        mintsExactlyOneToken = case flattenValue valueMinted of
            -- we ignore currencySymbol
            [(_, tn', amt)] ->  tn' == tn && amt == 1
            _               -> False

{-# INLINABLE mkWrappedDataTokenPolicy #-}
--                          Transactionid  TransactionIndex TokenName
mkWrappedDataTokenPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDataTokenPolicy txId ix tn = wrapPolicy $ mkDataTokenPolicy utxoRef tokenName
    where
        utxoRef :: TxOutRef
        utxoRef = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData txId) (PlutusTx.unsafeFromBuiltinData ix)

        tokenName :: TokenName
        tokenName = PlutusTx.unsafeFromBuiltinData tn

dataTokenMintingCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
dataTokenMintingCode = $$(PlutusTx.compile [|| mkWrappedDataTokenPolicy ||])

dataTokenMintingPolicy :: TxOutRef -> TokenName -> MintingPolicy
dataTokenMintingPolicy utxo tn = mkMintingPolicyScript $
    dataTokenMintingCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId utxo)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx utxo)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

------------------------------------------------------------
---- Helpers

-- Used by Lucid
saveDataTokenMintingCode :: IO ()
saveDataTokenMintingCode = writeCodeToFile "assets/dataToken.plutus" dataTokenMintingCode

saveDataTokenMintingPolicy :: TxOutRef -> TokenName -> IO ()
saveDataTokenMintingPolicy oref tn = writePolicyToFile
    (printf "assets/nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn') $
    dataTokenMintingPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

dataTokenCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
dataTokenCurrencySymbol oref tn = currencySymbol $ dataTokenMintingPolicy oref tn
