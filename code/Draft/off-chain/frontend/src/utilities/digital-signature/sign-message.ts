import { Lucid, SignedMessage, fromText } from 'lucid-cardano';

export const signMessage = async (lucid: Lucid, payload: string, wallet: string): Promise<SignedMessage> => {
  console.log('SIGNING MESSAGE');
  console.log('utf8Payload', payload);
  const hexPayload = fromText(payload);
  console.log('hexPayload', hexPayload);
  return lucid.wallet.signMessage(wallet, hexPayload);
};
