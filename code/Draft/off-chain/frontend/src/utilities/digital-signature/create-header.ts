import { SignedMessage } from 'lucid-cardano';

export const createDigitalSignatureHeader = (payload: string, signedMessage: SignedMessage): [string, string] => {
  const { signature, key } = signedMessage;

  // signature: string;
  // The public key
  // key: string;

  return ['Authorization', `Signature:${payload}:${signature}:${key}`];
};

export const WALLET_ADDRESS_HEADER = 'wallet-address';

export const createWalletHeader = (walletAddress: string): [string, string] => {
  return [WALLET_ADDRESS_HEADER, walletAddress];
};
