// pages/api/retrieveHistoryForBuyer.ts

import { DataSession } from '@/types';
import { decrypt } from '@/utilities/encryption';
import { storage } from '@/utilities/storage/index';
import crypto from 'crypto';
import { NextApiRequest, NextApiResponse } from 'next';

/**
 * Improvements:
 * - Authorization, demand owner of token sends a signed nonce timestamped
 * (we can validate the token's wallet owner server side)
 */
export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  const { tokenAssetClass, wAddr } = req.body;
  // TODO check that wAddr indeed contains that tokenAssetClass and using signed nonce timestamped, that client is owner of wAddr

  if (!tokenAssetClass || !wAddr) {
    return res.status(400).json({ error: 'Token asset class and wallet address are required' });
  }

  const result = await storage.retrieveData(tokenAssetClass as string);

  if (!result) {
    return res.status(404).json({ error: 'No data found for the given token asset class' });
  }

  const encryptionKey = process.env.ENCRYPTION_KEY;
  if (!encryptionKey) {
    console.error('Encryption key is missing');
    return res.status(500).json({ error: 'Internal server error' });
  }
  if (typeof result !== 'string') {
    console.error('Stored data are not encrypted');
    return res.status(500).json({ error: 'Internal server error' });
  }
  const decryptedData = decrypt(result, encryptionKey);
  // console.log('decryptedData', decryptedData);
  const dataSession: DataSession = JSON.parse(decryptedData);
  console.log({ dataSession });
  // console.log('Retrieving history result');
  res.status(200).json({ success: true, data: dataSession });
}
