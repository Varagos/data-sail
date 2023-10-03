import { Lucid, SignedMessage, fromText } from 'lucid-cardano';

export const signMessage = async (lucid: Lucid, payload: string, wallet: string): Promise<SignedMessage> => {
  return lucid.wallet.signMessage(wallet, fromText(payload));
};
