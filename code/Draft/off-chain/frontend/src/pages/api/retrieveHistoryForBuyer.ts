// pages/api/decrypt.js

import { decrypt } from '@/utilities/encryption';
import crypto from 'crypto';

export default async function handler(req, res) {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  const { encryptedData } = req.body;

  if (!encryptedData) {
    return res.status(400).json({ error: 'Encrypted data is required' });
  }

  const decryptionKey = process.env.ENCRYPTION_KEY;
  if (!decryptionKey) {
    console.error('Encryption key is missing');
    return res.status(500).json({ error: 'Internal server error' });
  }

  try {
    const decryptedData = decrypt(encryptedData, decryptionKey);
    res.status(200).json({ decryptedData });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'An error occurred while decrypting the data' });
  }
}
