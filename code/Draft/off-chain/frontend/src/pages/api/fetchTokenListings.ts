// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';
import { tokenListingStorageService } from '@/services/token-listings';

/**
 *  * Add validation rule to ensure that the wallet address is the same as the one that has this token asset class
 */
const fetchTokenListings = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }
  console.log('Associate data with token', req.body);

  const result = await tokenListingStorageService.fetchAllDataListings();

  res.status(201).json({ success: true, data: result });
};

export default fetchTokenListings;
