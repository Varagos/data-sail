import { NextApiRequest } from 'next';
import { parseDigitalSignatureHeader } from '../digital-signature/parse-header';
import { Lucid } from 'lucid-cardano';
import { verifyMessage } from '../digital-signature/verify-message';
import { WALLET_ADDRESS_HEADER } from '../digital-signature/create-header';

export type IsSignedByWalletGuardResult = {
  success: boolean;
  error?: string;
  error_code?: 'BAD_INPUT' | 'UNAUTHORIZED' | 'INTERNAL_SERVER_ERROR';
};

/**
 * Checks that the request contains a valid digital signature for the
 * wallet at X-Wallet-Address header
 */
export const isSignedByWalletGuard = (req: NextApiRequest, lucid: Lucid): IsSignedByWalletGuardResult => {
  const { authorization } = req.headers;
  if (!authorization) {
    console.error('Authorization header is missing');
    return {
      success: false,
      error: 'Authorization header is missing',
      error_code: 'BAD_INPUT',
    };
  }

  const parsedHeader = parseDigitalSignatureHeader(authorization);
  if (parsedHeader === null) {
    return {
      success: false,
      error: 'Invalid authorization header',
      error_code: 'BAD_INPUT',
    };
  }
  console.log(req.headers);
  console.log(WALLET_ADDRESS_HEADER);
  const walletAddress = req.headers[WALLET_ADDRESS_HEADER];
  if (!walletAddress || typeof walletAddress !== 'string') {
    console.error('Wallet address header is missing');
    return {
      success: false,
      error: 'Wallet address header is missing or invalid',
      error_code: 'BAD_INPUT',
    };
  }
  const { payload, signedMessage } = parsedHeader;
  // TODO could also check payload contains recent timestamp

  const isVerified = verifyMessage(lucid, signedMessage, payload, walletAddress);
  if (!isVerified) {
    return {
      success: false,
      error: 'Invalid signature',
      error_code: 'UNAUTHORIZED',
    };
  }

  return {
    success: true,
  };

  //   const isValid = await lucid.wallet.verifyMessage(fromText(payload), signedMessage);
};
