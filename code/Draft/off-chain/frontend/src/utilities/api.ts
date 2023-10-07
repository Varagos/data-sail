import type { TokenListing } from '@/services/token-listings/interface';
import type { DataSession } from '@/types';

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

export async function addTokenListing(walletAddr: string, dataTokenAssetClass: string) {
  const res = await fetch('/api/addTokenListing', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ walletAddr, tokenAssetClass: dataTokenAssetClass }),
  });

  if (res.ok) {
    console.log('Added token listing');
  } else {
    console.log('Error:', res.status);
  }
}

export async function fetchTokenListingsApi(): Promise<TokenListing[]> {
  const res = await fetch('/api/fetchTokenListings', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (res.ok) {
    const { data: tokenListings } = await res.json();
    console.log('Token Listings:', tokenListings);
    return tokenListings as TokenListing[];
  } else {
    console.log('Error:', res.status);
    throw new Error('Error fetching token listings');
  }
}
