import { Lucid, SignedMessage, fromText } from 'lucid-cardano';

export const verifyMessage = (lucid: Lucid, signature: SignedMessage, payload: string, wallet: string) => {
  // console.log('VERIFYING MESSAGE');
  // console.log('utf8Payload', payload);
  const hexPayload = fromText(payload);
  // console.log('hexPayload', hexPayload);
  return lucid.verifyMessage(wallet, hexPayload, signature);
};
