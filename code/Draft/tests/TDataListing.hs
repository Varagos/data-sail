{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad          (unless)
import qualified DataListing            as OnChain
import           Plutus.Model           (DatumMode (HashDatum),
                                         FakeCoin (FakeCoin), Run, Tx,
                                         TypedValidator (TypedValidator),
                                         UserSpend, adaValue, defaultBabbage,
                                         fakeValue, logError, mustFail, newUser,
                                         noErrors, payToKey, payToScript, spend,
                                         spendScript, submitTx, testNoErrors,
                                         toV2, userSpend, utxoAt, valueAt)
import           Plutus.V1.Ledger.Value (flattenValue)
import           Plutus.V2.Ledger.Api   (PubKeyHash, TxOut (txOutValue),
                                         TxOutRef, Value)
import           PlutusTx.Prelude       (($))
import           Prelude                (Eq ((==)), Functor (fmap), IO, Int,
                                         Integer, Monoid (mconcat), Num ((-)),
                                         Show (show),
                                         Traversable (mapM, sequence), (!!),
                                         (&&), (++), (.), (<>))
import           Test.Tasty             (defaultMain, testGroup)

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Catch double spend with testing"
      [ good "Normal spending" $ normalSpending 0 600 600
      , bad "Tries to pay less than asked" $ normalSpending 0 600 500
      , bad "Tries to pay someone else" $ normalSpending 1 600 600
      , bad  "Double spending" doubleSpending
      ]
 where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000 <> fakeValue scToken 100) defaultBabbage

---------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS/INSTANCES/TYPES ------------------------------------

scToken :: FakeCoin
scToken = FakeCoin "Super-Cool-Token"

type DataListingScript = TypedValidator OnChain.DataListDatum ()

swapScript :: DataListingScript
swapScript = TypedValidator $ toV2 OnChain.validator

-- | alocate 2 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = sequence [firstUser, secondUser]
  where
    firstUser = newUser $ fakeValue scToken 100
    secondUser = newUser $ adaValue 1000

-- Create transaction that locks scToken
lockingTx :: PubKeyHash -> Integer -> UserSpend -> Value -> Tx
lockingTx ph pr usp val=
  mconcat
    [
      userSpend usp, -- This spend the user inputs, and send the change back to the user
      payToScript swapScript datum val

    ]
  where
    datum = HashDatum (OnChain.DataListDatum ph pr)

consumingTx :: PubKeyHash -> PubKeyHash -> UserSpend -> Value -> TxOutRef -> Value -> OnChain.DataListDatum -> Tx
consumingTx buyer benef buyerSpending buyerPaying txOutRef tokensVal dat =
  mconcat
    [
     spendScript swapScript txOutRef () dat
    , payToKey buyer tokensVal -- Where to pay the unlocked value
    -- WE ALSO NEED TO PAY THE BENEFICIARY
    , userSpend buyerSpending
    , payToKey benef buyerPaying
    ]


doubleConsumingTx ::PubKeyHash -> PubKeyHash -> UserSpend -> Value -> (TxOutRef, TxOutRef) -> Value -> OnChain.DataListDatum -> Tx
doubleConsumingTx buyer benef buyerSpending buyerPaying (txOutRef1,txOutRef2) tokensVal dat =
  mconcat
    [
      spendScript swapScript txOutRef1 () dat
      , payToKey buyer tokensVal
      , spendScript swapScript txOutRef2 () dat
      , payToKey buyer tokensVal
      -- What the buyer is paying
      ,userSpend buyerSpending
      , payToKey benef buyerPaying
    ]

---------------------------------------------------------------------------------------------------
-------------------------------------- TESTING SPENDING -------------------------------------------

normalSpending :: Int -> Integer -> Integer -> Run ()
normalSpending benefIndex askedPrice paidPrice = do
  users <- setupUsers
  let [u1, u2] = users
  let benef = users !! benefIndex :: PubKeyHash
  -- User 1 locks his tokens
  let tokenVal = fakeValue scToken 100
  sp <- spend u1 tokenVal

  submitTx u1 $ lockingTx u1 askedPrice sp tokenVal

  -- Now time for user 2 to consume them
  scriptUtxos <- utxoAt swapScript
  let [(ref, out)] = scriptUtxos -- We have only locked 1 utxo


  let payingVal = adaValue paidPrice
  u2_sp <- spend u2 payingVal
  submitTx u2 $ consumingTx u2 benef u2_sp payingVal ref (txOutValue out) (OnChain.DataListDatum u1 askedPrice)

  isOk <- noErrors
  vals <- mapM valueAt users          -- read user values
  [v1, v2] <- mapM valueAt users
  unless (isOk &&
    v1 ==  adaValue askedPrice  &&
    v2 == fakeValue scToken 100 <> adaValue (1000 - askedPrice))
    $ logError $ "Error occured. Received values: "  ++ show  (fmap flattenValue vals)


-- We write this test expecting to succeed, since we declare above we want it as bad
-- So the abuser here, would want u1 to only get 400, even though he would sell 100 tokens
-- while user1 would normally expect to receive 400 * 2
doubleSpending :: Run ()
doubleSpending = do
  [u1,u2] <- setupUsers

  -- Lock 2 tokens utxos
  let token = fakeValue scToken 50
  sp1 <- spend u1 token
  submitTx u1 $ lockingTx u1 400 sp1 token

  sp2 <- spend u1 token
  submitTx u1 $ lockingTx u1 400 sp2 token
  --
  scriptsUtxos <- utxoAt swapScript
  let [(ref1, out1), (ref2, out2)] = scriptsUtxos

  let buyerPaying = adaValue 400
  u2_sp <- spend u2 buyerPaying
  submitTx u2 $ doubleConsumingTx u2 u1 u2_sp buyerPaying (ref1,ref2) (txOutValue out1) (OnChain.DataListDatum u1 400)

  [v1,v2] <- mapM valueAt [u1,u2]
  unless (v1 == adaValue 400 &&
    v2 == adaValue 600 <> fakeValue scToken 100)
    $ logError  $ "Error occured. Received values: "  ++ show  (fmap flattenValue [v1,v2])
