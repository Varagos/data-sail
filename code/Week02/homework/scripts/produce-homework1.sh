#!/bin/bash

assets=/workspace/code/Week02/assets
keypath=/workspace/keys
name="$1"
txin="$2"
body="$assets/gift.txbody"
tx="$assets/gift.tx"

# Build gift address, given the serialized script
cardano-cli address build \
    --payment-script-file "$assets/homework1.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/homework1.addr"

# Build the transaction, send to the gift addr generated from previous command.
# Will send 3 ada to the script address
# we must attach a datum, the simplest datum is that which corresponds the the Haskell unit value.
# We also give a change-address, where any funds not needed from the input utxo will be send, we send them back to the owner
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/gift.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/unit.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction
# The transaction is built, but now the sender has to sign it
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

# Given the transaction file, we get the transaction hash(id)
tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"