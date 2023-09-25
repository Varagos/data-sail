import { DataSession } from '@/types';

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

export async function retrieveHistoryForBuyer(tokenAssetClass: string, wAddr: string): Promise<DataSession> {
  const res = await fetch('/api/retrieveHistoryForBuyer', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ tokenAssetClass, wAddr }),
  });

  if (res.ok) {
    const { data: decryptedData } = await res.json();
    console.log('Decrypted Data:', decryptedData);
    return decryptedData as DataSession;
  } else {
    console.log('Error:', res.status, res);
    throw new Error('Error retrieving history for buyer');
  }
}
