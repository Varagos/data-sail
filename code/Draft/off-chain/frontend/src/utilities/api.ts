import type { TokenListing } from '@/services/token-listings/interface';
import type { DataSession } from '@/types';
import { signMessage } from './digital-signature/sign-message';
import { Lucid } from 'lucid-cardano';
import { createDigitalSignatureHeader, createWalletHeader } from './digital-signature/create-header';
import type { ActiveBid } from '@/services/active-bids/interface';

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

export async function fetchTokenListingsApi(): Promise<TokenListing[]> {
  const res = await fetch('/api/tokenListing/fetchAll', {
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

export class ActiveBidsApi {
  static async fetchActiveBids(wallet: string): Promise<ActiveBid[]> {
    const res = await fetch(`/api/activeBid/fetchByWallet?wallet=${wallet}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (res.ok) {
      const { data: activeBids } = await res.json();
      console.log('Active Bids:', activeBids);
      return activeBids as ActiveBid[];
    } else {
      console.log('Error:', res.status);
      throw new Error('Error fetching active bids');
    }
  }

  static async createActiveBid(wallet: string, activeBid: ActiveBid): Promise<void> {
    const res = await fetch('/api/activeBid/create', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ wallet, activeBid }),
    });

    if (res.ok) {
      console.log('Active bid created');
    } else {
      console.log('Error:', res.status);
      throw new Error('Error creating active bid');
    }
  }

  static async deleteActiveBid(wallet: string, bidId: string): Promise<void> {
    const res = await fetch('/api/activeBid/delete', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ wallet, bidId }),
    });

    if (res.ok) {
      console.log('Active bid deleted');
    } else {
      console.log('Error:', res.status);
      throw new Error('Error deleting active bid');
    }
  }
}
