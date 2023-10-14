import { NextApiRequest, NextApiResponse } from 'next';
import { redisTokenListings, tokenListingStorageService } from '@/services/token-listings';

const fetchTokenListings = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  const result = await redisTokenListings.fetchAllDataListings();

  res.status(201).json({ success: true, data: result });
};

export default fetchTokenListings;
