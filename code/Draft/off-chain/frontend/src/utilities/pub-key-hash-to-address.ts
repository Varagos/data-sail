import { UTxO, getAddressDetails } from 'lucid-cardano';

async function fetchTxInputAddresses(txHash: string) {
  try {
    const response = await fetch(`/api/getTxInfo?txHash=${txHash}`);
    const data = await response.json();
    return data;
  } catch (error) {
    console.error('An error occurred while fetching the transaction:', error);
    return null;
  }
}

/**
 * Find using txHash and some provider
 */
export const findSellerAddress = async (lockedUtxo: UTxO, pubKeyHash: string): Promise<string> => {
  const { txHash } = lockedUtxo;
  console.log({
    txHash,
  });
  // const tx = `${txHash}#${outputIndex}`;
  // Tx that locked the utxo, we are trying to find the address that locked it
  // const utxos = await API.txsUtxos(txHash);
  const txInfo = await fetchTxInputAddresses(txHash);
  const inputAddresses: string[] = txInfo.inputAddress;
  const addressesMatchingKeyHash = inputAddresses.filter(
    (addr) => getAddressDetails(addr).paymentCredential?.hash === pubKeyHash
  );
  if (addressesMatchingKeyHash.length === 0) {
    // Fallback to address directly from pubkeyhash
    throw new Error('No addresses found matching the key hash');
  }
  // Multiple addresses because of different staking credentials
  // Return the largest
  return addressesMatchingKeyHash.sort((a, b) => b.length - a.length)[0];

  // return txHash;
};
