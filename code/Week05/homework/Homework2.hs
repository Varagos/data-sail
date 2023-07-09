{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext (scriptContextTxInfo), TokenName (unTokenName), TxOutRef,
                                       mkMintingPolicyScript, TxInfo (txInfoInputs, txInfoMint), TxInInfo (txInInfoOutRef))
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.), traceIfFalse,  any, Eq ((==)), (&&))
import           Utilities            (wrapPolicy)
import Plutus.V1.Ledger.Value (flattenValue, )

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx = traceIfFalse "not a true nft" spendsUtxo &&
    traceIfFalse "Token name is not empty string" tokenNameIsEmptyString
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txInputs = txInfoInputs txInfo

        spendsUtxo :: Bool
        spendsUtxo = any (\x -> oref == txInInfoOutRef x) txInputs 

        tokenNameIsEmptyString = case flattenValue $ txInfoMint txInfo of
            [(_, t, amt)] -> unTokenName t == "" && amt ==1 
            _ -> False

-- Here we do the untyped version the usual simple way, but less well suited for off-chain code with lucid
{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
