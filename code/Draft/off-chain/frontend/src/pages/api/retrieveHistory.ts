// pages/api/saveHistory.js
import { DataSession } from '@/types';
import { WALLET_ADDRESS_HEADER } from '@/utilities/digital-signature/create-header';
import { decrypt } from '@/utilities/encryption';
import { isSignedByWalletGuard } from '@/utilities/guards/is-signed-by-wallet';
import { ipfsStorage, storage } from '@/utilities/storage/index';
import { Blockfrost, Lucid } from 'lucid-cardano';
import { NextApiRequest, NextApiResponse } from 'next';

const blockFrostKey = process.env.BLOCKFROST_API_KEY;
const lucid = await Lucid.new(new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blockFrostKey), 'Preview');

/**
 * Improvements:
 * - Authorization, the wallet owner could sign a nonce timestamped and send it to the server
 * , we will check the signature and the timestamp to make sure that the request is coming from the wallet owner
 */
const retrieveHistory = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  // console.log('Entered retrieveHistory');
  const { identifier } = req.query; // Extracting the identifier from the query string

  if (!identifier) {
    return res.status(400).json({ success: false, message: 'Identifier is required' });
  }

  const validationResult = isSignedByWalletGuard(req, lucid);
  if (validationResult.success === false) {
    return res.status(401).json({ success: false, message: validationResult.error });
  }
  const wallet = req.headers[WALLET_ADDRESS_HEADER];
  if (wallet !== identifier) {
    return res.status(400).json({ success: false, message: 'Identifier does not match wallet address' });
  }
  // console.log('Validated digital signature for wallet');

  const cid = await storage.retrieveData(identifier as string);
  if (!cid) {
    // return res.status(404).json({ success: false, message: 'No data found for the given identifier' });
    // Too many error logs, so let's just return an empty array
    return res.status(200).json({ success: true, data: [] });
  }

  const encryptionKey = process.env.ENCRYPTION_KEY;
  if (!encryptionKey) {
    console.error('Encryption key is missing');
    return res.status(500).json({ error: 'Internal server error' });
  }

  const result = await ipfsStorage.retrieveData(cid as string);
  // console.log('result', result);

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
  // console.log('Retrieving history result', result);
  // res.status(200).json({ success: true, data: result });
};

export default retrieveHistory;
