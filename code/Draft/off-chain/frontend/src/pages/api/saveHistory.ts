// pages/api/saveHistory.js
import crypto from 'crypto';
import { storage } from '@/utilities/storage/index';

// Gets invoked by the extension, passing the data and wallet address associated with the data
const saveHistory = async (req, res) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  console.log('req.body', req.body);
  const data = req.body.data; // Validate and sanitize the data as needed
  // const walletAddr = req.body.walletAddr; // Validate and sanitize the data as needed
  if (!data) {
    console.log('Missing params');
    return res.status(400).json({ error: 'Data are required' });
  }

  const encryptionKey = process.env.ENCRYPTION_KEY;

  if (!encryptionKey) {
    console.error('Encryption key is missing');
    return res.status(500).json({ error: 'Internal server error' });
  }

  const encryptedData = encrypt(data, encryptionKey);

  const identifier = await storage.storeData(encryptedData);
  console.log('Saving history result', identifier);

  res.status(201).json({ success: true, data: identifier });
};

// Encrypt a plaintext string using the AES-256-CBC algorithm
function encrypt(text: string, key: string) {
  // The Initialization Vector is generated randomly for each encryption and prepended to the encrypted data.
  const iv = crypto.randomBytes(16);
  // initialize the cipher object with the key and IV
  const cipher = crypto.createCipheriv('aes-256-cbc', Buffer.from(key), iv);
  let encrypted = cipher.update(text);
  // [cipher.final()] ensuring that any remaining bytes are processed
  encrypted = Buffer.concat([encrypted, cipher.final()]);
  // It's essential that the IV used for encryption is also used for decryption, which is why it's included in the output.
  return iv.toString('hex') + ':' + encrypted.toString('hex');
}

export default saveHistory;
