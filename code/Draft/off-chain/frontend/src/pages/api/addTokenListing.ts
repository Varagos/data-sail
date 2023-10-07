// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';
import { tokenListingStorageService } from '@/services/token-listings';

/**
 *  * Add validation rule to ensure that the wallet address is the same as the one that has this token asset class
 */
const addTokenListing = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  console.log('Associate data with token', req.body);

  const walletAddr = req.body.walletAddr; // Validate and sanitize the data as needed
  const dataTokenAssetClass = req.body.tokenAssetClass;
  if (!dataTokenAssetClass || !walletAddr) {
    console.log('Missing params');
    return res.status(400).json({ error: 'DataTokenAssetClass and walletAddr are required' });
  }

  await tokenListingStorageService.addTokenListing({
    owner: walletAddr,
    tokenAssetClass: dataTokenAssetClass,
  });

  res.status(201).json({ success: true });
};

export default addTokenListing;
