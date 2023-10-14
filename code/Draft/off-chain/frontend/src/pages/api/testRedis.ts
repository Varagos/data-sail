// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { redisTokenListings } from '@/services/token-listings';

const testRedis = async (req: NextApiRequest, res: NextApiResponse) => {
  console.log('Testing RedisTokenListings:');
  await redisTokenListings.addTokenListing({
    owner: '1331',
    tokenAssetClass: 'adjdad',
  });
  console.log('Saved token listing:');

  res.status(201).json({ success: true });
};

export default testRedis;
