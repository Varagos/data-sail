import { Lucid } from 'lucid-cardano';
import { NextApiRequest } from 'next';
import { isSignedByWalletGuard } from './is-signed-by-wallet';
import { WALLET_ADDRESS_HEADER } from '../digital-signature/create-header';
import { walletHasToken } from '../blockfrost/wallet-has-token';

export type IsTokenOwnerGuardResult = {
  success: boolean;
  error?: string;
  error_code?: 'BAD_INPUT' | 'UNAUTHORIZED' | 'INTERNAL_SERVER_ERROR';
};

/**
 * Checks that the request contains a valid digital signature for the
 * wallet at wallet-address header
 * AND that the wallet owns the given token asset class
 */
export const isTokenOwnerGuard = async (
  req: NextApiRequest,
  lucid: Lucid,
  tokenAssetClass: string
): Promise<IsTokenOwnerGuardResult> => {
  const validationResult = isSignedByWalletGuard(req, lucid);
  if (validationResult.success === false) {
    return {
      success: false,
      error: validationResult.error,
      error_code: validationResult.error_code,
    };
  }
  const wallet = req.headers[WALLET_ADDRESS_HEADER] as string;
  // Make sure wallet is owner of tokenAssetClass
  const walletHasTokenResult = await walletHasToken(wallet, tokenAssetClass);
  if (!walletHasTokenResult) {
    return {
      success: false,
      error: 'Wallet does not own the given token asset class',
      error_code: 'UNAUTHORIZED',
    };
  }

  return {
    success: true,
  };
};
