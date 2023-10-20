import { activeBidsStorageService, redisActiveBids } from '@/services/active-bids';
import type { NextApiRequest, NextApiResponse } from 'next';

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }
  console.info(`[${new Date().toLocaleTimeString()}]: GET /api/activeBid/fetchByWallet`);

  const { wallet } = req.query;

  try {
    // const bids = await activeBidsStorageService.fetchAllActiveBids(wallet as string);
    // console.log('Fetching bids for wallet: ', wallet);
    if (typeof wallet !== 'string') {
      return res.status(400).json({ error: 'Invalid wallet address' });
    }

    const bids = await redisActiveBids.fetchAllActiveBids(wallet);

    res.status(200).json({ data: bids });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'Internal server error' });
  }
}
