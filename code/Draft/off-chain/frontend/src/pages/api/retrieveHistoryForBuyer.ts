import { DataSession } from '@/types';
import { decrypt } from '@/utilities/encryption';
import { isTokenOwnerGuard } from '@/utilities/guards/is-token-owner';
import { ipfsStorage, storage } from '@/utilities/storage/index';
import { Blockfrost, Lucid } from 'lucid-cardano';
import { NextApiRequest, NextApiResponse } from 'next';

const blockFrostKey = process.env.BLOCKFROST_API_KEY;
const lucid = await Lucid.new(new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blockFrostKey), 'Preview');

/**
 * Retrieves history of data associated with the given token asset class
 */
export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  console.info('POST /api/retrieveHistoryForBuyer');

  const { tokenAssetClass } = req.body;

  if (!tokenAssetClass) {
    return res.status(400).json({ error: 'Token asset class and wallet address are required' });
  }

  const validationResult = await isTokenOwnerGuard(req, lucid, tokenAssetClass);
  if (validationResult.success === false) {
    return res.status(401).json({ error: validationResult.error });
  }

  const cid = await storage.retrieveData(tokenAssetClass);
  console.log('cid', cid);
  if (!cid) {
    return res.status(404).json({ error: 'No data found for the given token asset class' });
  }
  const result = await ipfsStorage.retrieveData(cid as string);
  // console.log('result', result);

  // const result = await storage.retrieveData(tokenAssetClass as string);

  if (!result) {
    console.error('No data found at ipfs for that cid');
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
  // console.log({ dataSession });
  // console.log('Retrieving history result');
  res.status(200).json({ success: true, data: dataSession });
}
