import { DataSession } from '@/types';
import { signMessage } from './digital-signature/sign-message';
import { Lucid } from 'lucid-cardano';
import { createDigitalSignatureHeader, createWalletHeader } from './digital-signature/create-header';

export async function associateDataWithToken(walletAddr: string, dataTokenAssetClass: string) {
  const res = await fetch('/api/associateDataWithToken', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ walletAddr, tokenAssetClass: dataTokenAssetClass }),
  });

  if (res.ok) {
    console.log('Associated data with token');
  } else {
    console.log('Error:', res.status);
  }
}

export async function retrieveHistoryForBuyer(
  tokenAssetClass: string,
  wAddr: string,
  lucid: Lucid
): Promise<DataSession> {
  const payload = 'Fetch_seller_data_' + Date.now();
  const signedMessage = await signMessage(lucid, payload, wAddr);
  const [signatureKey, signatureValue] = createDigitalSignatureHeader(payload, signedMessage);
  const [walletKey, walletValue] = createWalletHeader(wAddr);

  const headers = new Headers();
  headers.append(signatureKey, signatureValue);
  headers.append(walletKey, walletValue);
  headers.append('Content-Type', 'application/json');

  const res = await fetch('/api/retrieveHistoryForBuyer', {
    method: 'POST',
    headers: headers,
    body: JSON.stringify({ tokenAssetClass }),
  });

  if (res.ok) {
    const { data: decryptedData } = await res.json();
    // console.log('Decrypted Data:', decryptedData);
    return decryptedData as DataSession;
  } else {
    console.log('Error:', res.status, res);
    throw new Error('Error retrieving history for buyer');
  }
}
