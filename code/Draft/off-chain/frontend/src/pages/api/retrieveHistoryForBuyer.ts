// pages/api/decrypt.js

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

function decrypt(text: string, key: string) {
  const textParts = text.split(':');
  const iv = Buffer.from(textParts.shift()!, 'hex');
  const encryptedText = Buffer.from(textParts.join(':'), 'hex');
  const decipher = crypto.createDecipheriv('aes-256-cbc', Buffer.from(key), iv);
  let decrypted = decipher.update(encryptedText);
  decrypted = Buffer.concat([decrypted, decipher.final()]);
  return decrypted.toString();
}
