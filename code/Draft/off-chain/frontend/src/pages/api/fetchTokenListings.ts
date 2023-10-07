import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';
import { tokenListingStorageService } from '@/services/token-listings';

const fetchTokenListings = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  const result = await tokenListingStorageService.fetchAllDataListings();

  res.status(201).json({ success: true, data: result });
};

export default fetchTokenListings;
