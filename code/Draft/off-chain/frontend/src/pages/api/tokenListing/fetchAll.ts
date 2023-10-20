import { NextApiRequest, NextApiResponse } from 'next';
import { redisTokenListings } from '@/services/token-listings';

const fetchTokenListings = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }
  console.info('GET /api/tokenListing/fetchAll');

  const result = await redisTokenListings.fetchAllDataListings();

  res.status(201).json({ success: true, data: result });
};

export default fetchTokenListings;
