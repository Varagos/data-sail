import { redisTokenListings } from '@/services/token-listings';
import type { NextApiRequest, NextApiResponse } from 'next';

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  console.info('DELETE /api/tokenListing/delete');
  const { tokenAssetClass } = req.body;

  try {
    await redisTokenListings.removeTokenListing(tokenAssetClass as string);
    res.status(200).json({ message: `Deleted token listing for ${tokenAssetClass}` });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'Internal server error' });
  }
}
