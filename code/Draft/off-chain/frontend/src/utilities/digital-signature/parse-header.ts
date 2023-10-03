import { SignedMessage } from 'lucid-cardano';

export const parseHeader = (headerValue: string): { payload: string; signedMessage: SignedMessage } | null => {
  const [scheme, payload, signature, key] = headerValue.split(':');
  if (scheme !== 'Signature') {
    console.error('Invalid scheme received for header value', scheme);
    return null;
  }
  return {
    signedMessage: {
      signature,
      key,
    },
    payload,
  };
};
