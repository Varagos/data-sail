import { NextApiRequest, NextApiResponse } from 'next';
import { encrypt } from '@/utilities/encryption';
import { ipfsStorage, storage } from '@/utilities/storage/index';

// Gets invoked by the extension, passing the data and wallet address associated with the data
const saveHistory = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  console.info(`[${new Date().toLocaleTimeString()}]: POST /api/saveHistory`);

  const data = req.body.data;
  const walletAddr = req.body.walletAddr;
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

  const cid = await ipfsStorage.storeData(encryptedData);

  const identifier = await storage.storeData(cid, walletAddr);

  res.status(201).json({ success: true, data: identifier });
};

export default saveHistory;
