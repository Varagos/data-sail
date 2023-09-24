// pages/api/saveHistory.js
import { DataSession } from '@/types';
import { decrypt } from '@/utilities/encryption';
import { storage } from '@/utilities/storage/index';

const retrieveHistory = async (req: any, res: any) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  console.log('Entered retrieveHistory');
  const { identifier } = req.query; // Extracting the identifier from the query string

  if (!identifier) {
    return res.status(400).json({ success: false, message: 'Identifier is required' });
  }

  const result = await storage.retrieveData(identifier);
  if (!result) {
    return res.status(404).json({ success: false, message: 'No data found for the given identifier' });
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
  console.log('decryptedData', decryptedData);
  const dataSession: DataSession = JSON.parse(decryptedData);
  console.log({ dataSession });
  // console.log('Retrieving history result');
  res.status(200).json({ success: true, data: dataSession });
  // console.log('Retrieving history result', result);
  // res.status(200).json({ success: true, data: result });
};

export default retrieveHistory;
