import crypto from 'crypto';

// Encrypt a plaintext string using the AES-256-CBC algorithm
export function encrypt(text: string, key: string) {
  // The Initialization Vector is generated randomly for each encryption and prepended to the encrypted data.
  const iv = crypto.randomBytes(16);
  // console.log(Buffer.from(key, 'hex').length);

  // initialize the cipher object with the key and IV
  const cipher = crypto.createCipheriv('aes-256-cbc', Buffer.from(key, 'hex'), iv);
  let encrypted = cipher.update(text);
  // [cipher.final()] ensuring that any remaining bytes are processed
  encrypted = Buffer.concat([encrypted, cipher.final()]);
  // It's essential that the IV used for encryption is also used for decryption, which is why it's included in the output.
  return iv.toString('hex') + ':' + encrypted.toString('hex');
}

export function decrypt(text: string, key: string) {
  const textParts = text.split(':');
  const iv = Buffer.from(textParts.shift()!, 'hex');
  const encryptedText = Buffer.from(textParts.join(':'), 'hex');
  const decipher = crypto.createDecipheriv('aes-256-cbc', Buffer.from(key, 'hex'), iv);
  let decrypted = decipher.update(encryptedText);
  decrypted = Buffer.concat([decrypted, decipher.final()]);
  return decrypted.toString();
}
