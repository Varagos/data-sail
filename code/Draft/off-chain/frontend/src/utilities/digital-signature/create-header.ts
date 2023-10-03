import { SignedMessage } from 'lucid-cardano';

export const createHeader = (payload: string, signedMessage: SignedMessage): [string, string] => {
  const { signature, key } = signedMessage;

  // signature: string;
  // The public key
  // key: string;

  return ['Authorization', `Signature:${payload}:${signature}:${key}`];
};
