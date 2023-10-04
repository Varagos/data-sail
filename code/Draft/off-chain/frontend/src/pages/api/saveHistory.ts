// pages/api/saveHistory.js
import { NextApiRequest, NextApiResponse } from 'next';
import { encrypt } from '@/utilities/encryption';
import { storage } from '@/utilities/storage/index';

// Gets invoked by the extension, passing the data and wallet address associated with the data
const saveHistory = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  console.log('req.body', req.body);
  const data = req.body.data; // Validate and sanitize the data as needed
  const walletAddr = req.body.walletAddr; // Validate and sanitize the data as needed
  if (!data || !walletAddr) {
    console.log('Missing params');
    return res.status(400).json({ error: 'Data and walletAddr are required' });
  }

  const encryptionKey = process.env.ENCRYPTION_KEY;

  if (!encryptionKey) {
    console.error('Encryption key is missing');
    return res.status(500).json({ error: 'Internal server error' });
  }

  const stringifiedData = JSON.stringify(data);
  const encryptedData = encrypt(stringifiedData, encryptionKey);

  const identifier = await storage.storeData(encryptedData, walletAddr);
  console.log('Saving history result', identifier);

  res.status(201).json({ success: true, data: identifier });
};

export default saveHistory;

// const generateEncryptionKey = () => {
// const encryptionKey = crypto.randomBytes(32).toString('hex');
// console.log('Generated 32-byte key:', encryptionKey);
// }
