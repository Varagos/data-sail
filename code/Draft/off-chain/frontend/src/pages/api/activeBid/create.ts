import { activeBidsStorageService, redisActiveBids } from '@/services/active-bids';
import type { NextApiRequest, NextApiResponse } from 'next';

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  try {
    const { wallet, activeBid } = req.body;
    await activeBidsStorageService.addActiveBid(wallet, activeBid);
    await redisActiveBids.addActiveBid(wallet, activeBid);
    res.status(201).json({ message: 'Active bid created' });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'Internal server error' });
  }
}
