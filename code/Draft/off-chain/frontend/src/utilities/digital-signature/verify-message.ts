import { Lucid, SignedMessage, fromText } from 'lucid-cardano';

export const verifyMessage = (lucid: Lucid, signature: SignedMessage, payload: string, wallet: string) => {
  return lucid.verifyMessage(wallet, fromText(payload), signature);
};
